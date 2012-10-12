# GHC plugin for Constant Math

This plugin eliminates constant math expressions from Haskell source.

Currently it replaces chains of unary expressions on constant Double or Float values
with the result of that expression. Supported expressions include all unary RealFloat
functions (exp, log, sqrt, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh,
atanh) and unary Num functions (negate, abs, signum).

To use it:

  $ cabal install

  $ ghc -fplugin ConstMath.Plugin foo.hs

Arguments can be passed to the plugin with

  $ ghc -fplugin ConstMath.Plugin -fplugin-opt=ConstMath.Plugin:ARG foo.hs

currently available arguments are
  -v,--verbose
      verbose output (default)

  --trace
      very verbose output (useful for seeing the AST used for matching)

  -q, --quiet
      no output

# More dakka:

More dakka is now built-in.  ConstMath.Rules does nothing, and will be removed
in a future release.

[travis-ci.org](http://travis-ci.org) results: [![Build
Status](https://secure.travis-ci.org/kfish/const-math-ghc-plugin.png?branch=master)](http://travis-ci.org/kfish/const-math-ghc-plugin)

[Homepage][main page].

# Installation

Install the latest version of the plugin from Hackage (requires GHC >= 7.4.1):

    $ cabal install const-math-ghc-plugin

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/kfish/const-math-ghc-plugin.git`

# License

BSD3. See `LICENSE` for terms of copyright and redistribution.

[main page]: http://kfish.github.com/const-math-ghc-plugin
[issue tracker]: http://github.com/kfish/const-math-ghc-plugin/issues
[gh]: http://github.com/kfish/const-math-ghc-plugin
