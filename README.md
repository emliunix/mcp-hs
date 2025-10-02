## MCP Implementation in Haskell

This is mainly a personal interest project.

JSON-RPC is a bidirectional communication protocol, which means it doesn't differentiate much between server and client.
However, MCP defines streamable HTTP as a transport for JSON-RPC, and HTTP is a client-server protocol.

Emulating such bidirectional communication on top of HTTP imposes some implementation requirements, making it worthwhile to write from scratch.

Here are some of the problems with HTTP I've noticed:

### Client to Server Response

SSE (Server-Sent Events) is a one-directional messaging protocol, so if a request goes through SSE (from server to client),
the corresponding response (from client to server) needs to be POSTed back,
which is another HTTP request. This requires some means of correspondence to connect the two HTTP request handlers.

This is trivial for a single-process server. When used as defined in MCP, the MCP server is started and managed locally by the user, which works fine.
However, I think MCP should not be limited to running only on the user's local machine. It should also be possible to host an MCP service like other API services.

Therefore, like classic HTTP services, cross-node sessions should be implemented. 
But unlike classical HTTP services, where sessions are usually backed by an RDBMS, 
some means of real-time message pushing is required for forwarding the aforementioned POSTed-back responses.

### Complicated SSE Response Logic

The POST handling is quite complex.

HTTP is more than just payloads—it has headers, methods, and status codes. These must follow the semantics of the payload.
For example, if there's no response data (a notification), the status code needs to be 202. If it's an error, the status code should be in the 400 range, or possibly 500. Additionally, there's the Mcp-Session-Id header for the session ID and the Mcp-Protocol-Version header for the protocol version.

Since all messages are now POSTed to the same endpoint, you have to branch based on whether it's a request or a response. When it's a request, depending on the handling, the response may be promoted to an SSE stream if demanded.

When it's SSE, it doesn't simply mean it's a text/event-stream with just some data records. You have to implement blocking, or in other words, a means to expose to the application code the capability of sending requests and resuming on responses (with proper timeouts).

---

Overall, it's a simple protocol. My next steps are to experiment with LLM agents and other functionalities using it.
