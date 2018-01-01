# Spaceleak detection

Every large Haskell program almost inevitably contains [space leaks](https://queue.acm.org/detail.cfm?id=2538488). Space leaks are often difficult to detect, but relatively easy to fix once detected (typically insert a <code>!</code>). To find a space leak, given a program and a representative run (e.g. the test suite, a suitable input file):

* Compile the program for profiling, e.g. `ghc --make Main.hs -rtsopts -prof -auto-all`.
* Run the program with a specific stack size, e.g. `./Main +RTS -K100K` to run with a 100Kb stack.
* Increase/decrease the stack size until you have determined the minimum stack for which the program succeeds, e.g. `-K33K`.
* Reduce the stack by a small amount and rerun with `-xc`, e.g. `./Main +RTS -K32K -xc`.
* The `-xc` run will print out the stack trace on every exception, look for the one which says `stack overflow` (likely the last one) and look at the stack trace to determine roughly where the leak is.
* Attempt to fix the space leak, confirm by rerunning with `-K32K`.
* Repeat until the test works with a small stack, typically `-K1K`.
* Add something to your test suite to ensure that if a space leak is ever introduced then it fails, e.g. `ghc-options: -with-rtsopts=-K1K` in Cabal.

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

### Fixes

* Making mapM take O(1) stack: http://neilmitchell.blogspot.co.uk/2015/09/making-sequencemapm-for-io-take-o1-stack.html
* Detecting space leaks (leak in base): http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html

### Code changes

* Happy: Space leaks https://github.com/simonmar/happy/pull/64 and improved code https://github.com/simonmar/happy/pull/66
* Pretty: https://github.com/haskell/pretty/pull/35
* Statistics: https://github.com/bos/statistics/pull/114

### Publications

* ACM: http://queue.acm.org/detail.cfm?id=2538488

### Notes

On the main thread the stack limit is less effective, usually more like 8K if you request 1K. On spawned threads it seems much better. Solution is to always `join . onceFork` on the main thread.
