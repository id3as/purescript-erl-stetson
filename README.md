# purescript-erl-stetson

Opinionated Bindings to Cowboy

## Type-safe bindings

Direct bindings to Cowboy's API exist in the package 'erl-cowboy', but in practise this are unwieldy to build an application with - this module seeks to provide a more functional set of behaviours around the low level bindings - more sspecifically, the more REST oriented side of things.

So for example, a complete web server serving some static content would look like this

## A complete web server

```purescript

  Stetson.startClear "http_listener"
    $ Stetson.configure
        { routes =
          Stetson.routes2 TestRoutes.apiRoute
            { "Root": rootHandler
            }
        , bindPort = 3000
        }

-- Perhaps the simplest possible handler
rootHandler :: StetsonHandler Unit Unit
rootHandler =
  routeHandler
    { init: \req -> Rest.initResult req unit
    , allowedMethods: \req state -> do Rest.result (Stetson.GET : nil) req state
    , contentTypesProvided: (\req state -> Rest.result (jsonWriter : nil) req state)
    }
```

The test directory contains a simple server (testStetsonConfig).  There is a much fuller example in the [demo project](https://github.com/id3as/demo-ps)

Disclaimer
==

This software, and the opinionated libraries written to support it are very much "works in progress" - we are actively using and building these libraries out for use in own commercial software and can and will be making any changes required to further support that development. As such, they come without support and a disclaimer very much of "be it on your own heads". That said - feel free to reach out and talk to us if you have ideas though, improvements and suggestions are welcome in pull requests and conversation.


