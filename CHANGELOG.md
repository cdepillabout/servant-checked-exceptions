
## 1.0.0.0

*   Add a `ErrStatus` class that can be used to set the HTTP Status Code. Given
    an endpoint that returns a `Envelope '[e1, e2] a`, you must declare an
    instance of `ErrStatus` for `e1` and `e2`.  This is a breaking change.

## 0.4.1.0

*   Add `NoThrow` type to represent handlers that don't throw any errors, but
    do return a result wrapped in an `Envelope`.
