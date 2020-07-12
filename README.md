# Spaceleak detection

Every large Haskell program almost inevitably contains [space leaks](https://queue.acm.org/detail.cfm?id=2538488). Space leaks are often difficult to detect, but relatively easy to fix once detected (typically insert a `!`). This page gives a simple technique to detect some space leaks, along with a set of blog posts using that technique and detailing other space leaks.

## Spaceleak stack-limiting technique

The stack-limiting technique is useful for detecting a common kind of spaceleak, a situation where an excessive number of interdependent thunks are created and eventually evaluated. The technique works because thunks are evaluated on the stack, so by limiting the stack size we effectively limit the number of interdependent thunks that we can evaluate.

To find space leaks, given a program and a representative run (e.g. the test suite, a suitable input file):

* Compile the program for profiling, e.g. `ghc --make Main.hs -rtsopts -prof -fprof-auto`.
* Run the program with a specific stack size, e.g. `./Main +RTS -K100K` to run with a 100Kb stack.
* Increase/decrease the stack size until you have determined the minimum stack for which the program succeeds, e.g. `-K33K`.
* Reduce the stack by a small amount and rerun with `-xc`, e.g. `./Main +RTS -K32K -xc`.
* The `-xc` run will print out the stack trace on every exception, look for the one which says `stack overflow` (likely the last one) and look at the stack trace to determine roughly where the leak is.
* Attempt to fix the space leak, confirm by rerunning with `-K32K`.
* Repeat until the test works with a small stack, typically `-K1K`.
* Add something to your test suite to ensure that if a space leak is ever introduced then it fails, e.g. `ghc-options: -with-rtsopts=-K1K` in Cabal.

This technique does not detect when an excessive number of thunks are created but never evaluated, or when a small number of thunks hold on to a large amount of live data.

Note that typically the main thread requires greater than 1K stack, and that once GHC crosses 1K it makes the stack 32K bigger, as a result anything less than 33K (e.g. 1K) is often rounded up to 33K. To obtain a more precise stack measurement use `-kc2k` which increases the stack in 2K chunks (but does cause only half the stack to be used, so bear that in mind when scaling `-K` numbers). That said, 32K corresponds to approximately 1000 stack evaluation slots, which suggests you don't have any significant space leaks.

Below are links to further information, including lots of practical examples of real space leaks caught using the method above.

### Talks

* Haskell eXchange 2016: Video https://skillsmatter.com/skillscasts/8724-plugging-space-leaks-improving-performance, slides http://ndmitchell.com/downloads/slides-plugging_space_leaks_improving_performance-06_oct_2016.pdf

### Blog tales

* QuickCheck: http://neilmitchell.blogspot.co.uk/2016/05/another-space-leak-quickcheck-edition.html
* Shake: http://neilmitchell.blogspot.co.uk/2013/02/chasing-space-leak-in-shake.html
* Detecting space leaks (leak in base): http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html
* Three space leaks (2x Hoogle, 1x Shake): http://neilmitchell.blogspot.co.uk/2015/09/three-space-leaks.html
* QuickCheck space leak: http://neilmitchell.blogspot.co.uk/2016/05/another-space-leak-quickcheck-edition.html
* Alex/Happy: http://neilmitchell.blogspot.co.uk/2016/07/more-space-leaks-alexhappy-edition.html
* Writer is a space leak: https://blog.infinitenegativeutility.com/2016/7/writer-monads-and-space-leaks (fixed by [writer-cps-transformers](https://hackage.haskell.org/package/writer-cps-transformers))
* Fixing 17 space leaks in GHCi: https://simonmar.github.io/posts/2018-06-20-Finding-fixing-space-leaks.html

### Fixes

* Making mapM take O(1) stack: http://neilmitchell.blogspot.co.uk/2015/09/making-sequencemapm-for-io-take-o1-stack.html
* Detecting space leaks (leak in base): http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html

### Code changes

* Happy: Space leaks https://github.com/simonmar/happy/pull/64 and improved code https://github.com/simonmar/happy/pull/66
* Pretty: https://github.com/haskell/pretty/pull/35
* Statistics: https://github.com/bos/statistics/pull/114

### Publications

* ACM: http://queue.acm.org/detail.cfm?id=2538488

### Other approaches

* http://simonmar.github.io/posts/2018-06-20-Finding-fixing-space-leaks.html describes how to use weak references to detect what memory is being retained.
* https://neilmitchell.blogspot.com/2020/05/fixing-space-leaks-in-ghcide.html describes finding space leaks in Ghcide and unordered-containers.
* https://github.com/haskell-unordered-containers/unordered-containers/issues/254#issuecomment-636387493 describes a trick about using `(# a #)` unboxed tuples as the return type of a function to keep laziness but get rid of space leaks.

### Notes

On the main thread the stack limit is less effective, usually more like 8K if you request 1K. On spawned threads it seems much better. Solution is to always `join . onceFork` on the main thread.
