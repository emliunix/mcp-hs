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

### Complicated SSE response logic

The POST handling is huge.

HTTP is more than payloads, it has headers, methods, status code. And these have to follow the semantics of the payload.
eg. If no response data (notification), status code needs to be 202. If it's an error, the status code should be one of the 400, or maybe 500. And Mcp-Session-Id header the session id, Mcp-Protocol-Version header the protocol version.

And because now all messages are POSTed to the same endpoint, you have to branch if it's a request or response. When it's a request, depending on the handling, the response maybe promoted to a SSE stream if demanded.

When it's SSE, it doesn't mean it's simply a text/event-stream that's just some data records. You have to implement blocking, or say a means to expose to the application code the capability of sending request and resume on response (and proper timeout).

---

Overall it's a simple protocol. And I think my next moves are to experiment with LLM agents and other functionalities using it.
