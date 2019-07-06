# purescript-erl-stetson

Opinionated Bindings to Cowboy

## Type-safe bindings

Direct bindings to Cowboy's API exist in the package 'erl-cowboy', but in practise this are unwieldy to build an application with - this module seeks to provide a more functional set of behaviours around the low level bindings - more sspecifically, the more REST oriented side of things.

So for example, a complete web server serving some static content would look like this

## A complete web server 

```purescript
                      
  Stetson.configure                       -- Start configuring Stetson
    # Stetson.route "/" routeHandler      -- Define a route, invoke 'routeHandler' to find out about it
    # Stetson.port 8080                   -- Listen on port 8080
    # Stetson.bindTo 0 0 0 0              -- And all interfaces
    # Stetson.startClear "http_listener"  -- Start the listener

-- Our route handler..
routeHandler :: StetsonHandler Unit -- Has a type of 'Unit' - this is the state that gets passed around to all callbacks 
routeHandler =
  Rest.handler (\req -> Rest.initResult req unit)  -- Callback invoked on init, define the initial state (in this case, 'unit')
    # Rest.contentTypesProvided (\req state -> Rest.result (tuple2 "text/html" writeText : nil) req state) -- Callback to provide list of handlers for different content types
    # Rest.yeeha -- Finish defining the handler
  where 
    writeText req state = do -- Callback invoked when we want to serve text/html as above
      Rest.result "Hello World" req state) -- The result of providing text/html, Hello World

```

An actual example with more context can be found in the [demo project](https://github.com/id3as/demo-ps)

