## 2.1.0.0

*   Add support for servant-0.16 and remove support for all previous version of
    servant. [#31](https://github.com/cdepillabout/servant-checked-exceptions/pull/31)
    Thanks [Schell Carl Scivally](https://github.com/schell)!

## 2.0.0.0

*   Split into two package `servant-checked-exceptions-core` and
    `servant-checked-exceptions`. The former defines the core types
    and functions for using checked exceptions in a servant API;
    the latter reexports the former and adds instances for `HasServer`
    and `HasClient`. The rationale is described further in
    [issue 25](https://github.com/cdepillabout/servant-checked-exceptions/issues/25)

    Most users should only depend on `servant-checked-exceptions`.
    But users who need access to core types without incurring a dependency
    on `servant-server` and `servant-client` can depend on
    `servant-checked-exceptions-core` instead.

*   Split `Exceptions` module into `Envelope` and `Verbs` in
    `servant-checked-exceptions-core`, for better module organization.
    More information in
    [issue 18](https://github.com/cdepillabout/servant-checked-exceptions/issues/18)

## 1.1.0.0

*   Updated the servant dependency to >= 0.12.

## 1.0.0.0

*   Add a `ErrStatus` class that can be used to set the HTTP Status Code. Given
    an endpoint that returns a `Envelope '[e1, e2] a`, you must declare an
    instance of `ErrStatus` for `e1` and `e2`.  This is a breaking change.

## 0.4.1.0

*   Add `NoThrow` type to represent handlers that don't throw any errors, but
    do return a result wrapped in an `Envelope`.
