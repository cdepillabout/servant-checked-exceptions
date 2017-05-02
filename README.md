
Servant.Checked.Exceptions
==========================

[![Build Status](https://secure.travis-ci.org/cdepillabout/servant-checked-exceptions.svg)](http://travis-ci.org/cdepillabout/servant-checked-exceptions)
[![Hackage](https://img.shields.io/hackage/v/servant-checked-exceptions.svg)](https://hackage.haskell.org/package/servant-checked-exceptions)
[![Stackage LTS](http://stackage.org/package/servant-checked-exceptions/badge/lts)](http://stackage.org/lts/package/servant-checked-exceptions)
[![Stackage Nightly](http://stackage.org/package/servant-checked-exceptions/badge/nightly)](http://stackage.org/nightly/package/servant-checked-exceptions)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

`servant-checked-exceptions` provides a way to specify errors thrown by a
Servant api on the type level.  It allows easy composition between different
error types.

For documentation and usage examples, see the
[documentation](https://hackage.haskell.org/package/servant-checked-exceptions) on Hackage.

## Example

This repository contains an [example](example/) of using
`servant-checked-exceptions`.  This includes an [api](example/Api.hs),
[server](example/Server.hs), [client](example/Client.hs), and
[documentation](example/Docs.hs).

Below I show how to compile and run these examples.

### Compile

The examples can be compiled by using the `buildexample` flag:

```sh
$ stack build --flag servant-checked-exceptions:buildexample
```

This creates three executables.  A server, a client, and a documentaiton
generator.

### Run the server

The server is a small example that will take search queries and return results.
The server can be run with the following command:

```sh
$ stack exec -- servant-checked-exceptions-example-server
```

This runs the server on port 8201.  Here is an example of using `curl` to
access the server.  This will send the query `hello`:

```sh
$ curl \
    --request POST \
    --header 'Accept: application/json' \
    'http://localhost:8201/lax-search/hello'
{"data":"good"}
```

If you try to send a query that is not `hello`, the server will return an error:

```sh
$ curl \
    --request POST \
    --header 'Accept: application/json' \
    'http://localhost:8201/lax-search/hello'
{"err":"BadSearchTermErr"}
```

There is also a strict api, that requires `hello` to be capitalized like `Hello`:

```sh
$ curl \
    --request POST \
    --header 'Accept: application/json' \
    'http://localhost:8201/strict-search/hello'
{"err":"IncorrectCapitalization"}
$ curl \
    --request POST \
    --header 'Accept: application/json' \
    'http://localhost:8201/strict-search/Hello'
{"data":"good"}
```

### Run the client

The client provides a small command line application to query the server.  In
order to use the client, the server must be running.

Use the client to access the lax search api:

```sh
$ stack exec -- servant-checked-exceptions-example-client foobar
the search term was not "Hello"
$ stack exec -- servant-checked-exceptions-example-client hello
Success: good
```

Use the client to access the strict search api:

```sh
$ stack exec -- servant-checked-exceptions-example-client --strict hello
the search term was not capitalized correctly
$ stack exec -- servant-checked-exceptions-example-client --strict Hello
Success: good
```

### Run the documentation generator

The documentation generator will generate documentation for the api in Markdown:

```sh
$ stack exec -- servant-checked-exceptions-example-docs
```

Here is a small example of the documentation that will be generated for the lax
search api:

```markdown
## POST /lax-search/:query

#### Captures:

- *query*: a search string like "hello" or "bye"

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- This is a successful response.

{"data":"good"}

- a completely incorrect search term was used

{"err":"BadSearchTermErr"}
```

You can see that both the success and error responses are documented.
