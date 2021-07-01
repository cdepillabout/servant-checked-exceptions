
## 2.2.0.1

*   Disable doctests for GHC-9. [#39](https://github.com/cdepillabout/servant-checked-exceptions/issues/39)

## 2.2.0.0

*   Add the `EnvelopeT` monad transformer. [#32](https://github.com/cdepillabout/servant-checked-exceptions/pull/32)

*   Add a few combinators for `Envelope`:

    - `envelopeRemove`
    - `envelopeHandle`
    - `relaxEnvelope`
    - `liftA2Envelope`
    - `bindEnvelope`

    [#32](https://github.com/cdepillabout/servant-checked-exceptions/pull/32)

## 2.1.0.0

*   Add support for servant-0.16 and remove support for all previous version of
    servant. [#31](https://github.com/cdepillabout/servant-checked-exceptions/pull/31)
    Thanks [Schell Carl Scivally](https://github.com/schell)!

## 2.0.0.0

*    Initial release of `servant-checked-exceptions-core` package, with
     core types factored out of `servant-checked-exceptions` for users
     who want access to them without incurring a dependency on `servant-server`
     and `servant-client`. See
    [issue 25](https://github.com/cdepillabout/servant-checked-exceptions/issues/25)

*    Compared to `servant-checked-exceptions`, `servant-checked-exceptions-core`
     breaks up the `Exceptions` module into `Verbs` and `Envelope`.
    [issue 18](https://github.com/cdepillabout/servant-checked-exceptions/issues/18)
