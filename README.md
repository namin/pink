# Pink in Scheme (Collapsing Towers of Interpreters)

## Code
* __[`base.scm`](base.scm)__ defines the multi-level core language λ↑↓ as a definitional interpreter in Scheme.
* __[`pink.scm`](pink.scm)__ defines the meta-circular stage-parametric interpreter for Pink on top of the base.
* __[`matcher.scm`](matcher.scm)__ defines a matcher as an example on top of Pink.
* __[`mk.scm`](mk.scm)__ defines a µKanren as an example on top of Pink.

## Run
Each code file `.scm` above has a companion `-tests.scm`, which can be run with [Chez Scheme](https://cisco.github.io/ChezScheme/).
For example, `chez pink-tests.scm` runs all the Pink-related tests.

## See Also

* [Pink in Scala](http://popl18.namin.net)
