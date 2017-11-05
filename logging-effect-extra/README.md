# [logging-effect-extra][]

## Synopsis

`logging-effect-extra` supplements [`logging-effect`][] with the following re-exported packages:

* `logging-effect-extra-file` - Convenient TH splices for adding file info to log messages

`logging-effect-extra` is a "batteries-included" package.  Each of the packages above can be used independently or in any combination without depending on `logging-effect-extra`.  For example, if Template Haskell is not acceptable for a project, users can depend on the other `logging-effect-extra-*` packages excluding `logging-effect-extra-file`.

This package has no clashing identifiers with [`logging-effect`][] so users may prefer importing this package and [`logging-effect`][] as follows:

```haskell
import qualified Control.Monad.Log as Log
import qualified Control.Monad.Log.Extra as Log
```

[logging-effect-extra]: https://github.com/jship/logging-effect-extra
[`logging-effect`]: https://github.com/ocharles/logging-effect
