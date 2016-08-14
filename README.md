# spaceleak


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

### Publications

* ACM: http://queue.acm.org/detail.cfm?id=2538488

### Notes

On the main thread the stack limit is less effective, usually more like 8K if you request 1K. On spawned threads it seems much better. Solution is to always `join . onceFork` on the main thread.
