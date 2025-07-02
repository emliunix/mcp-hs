## MCP implementation in Haskell

Mainly for personal interest.

JSON-RPC is bidi communication protocol which means it doesn't differentiate much between server and client.
But MCP defines streamable HTTP as a transport for JSON-RPC, and HTTP is a client server protocol.

Emulates such bidi communication on top of HTTP impose some implementation requirements so it's worth writing it from scratch.

Here're some of the problems with HTTP I noticed:

### client to server response

SSE is one direction messaging, so if request goes in SSE (from server to client),
the corresponding response (from client to server) needs to be POSTed back,
which is another request. So some means of correspondence is required to connect the two HTTP request handling.

And that's trivial for a single process server. If used as defined in MCP, MCP server is started and managed local to the user, it's fine.
But I think MCP should not be limited to live only on user's local machine. It's also possible to host a MCP service like other API services.

So like classic HTTP services, cross nodes session should be implemented. 
But unlike classical HTTP services, in which session is usually backed by a RDBMS, 
some means of realtime message pushing is required for the aforementioned POSTed back response forwarding.

---

Overall it's a simple protocol. And I think my next moves are to experiment with LLM agents and other functionalities using it.
