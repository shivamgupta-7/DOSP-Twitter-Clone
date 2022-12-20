# DOSP-Twitter-Clone
Developed as Part of Coursework for COP5615 - Distributed Operating Systems

https://user-images.githubusercontent.com/47007121/208740967-c5dc5787-e608-492d-905b-51faf9e6d072.mp4

Video and Audio Description-
https://youtu.be/noxN0aTtiGs

Used Cowboy web framework to implement a WebSocket interface to the part I implementation. That means that, even though the Erlang implementation (Part I) could use distributed actors messaging to allow client-server implementation, this part is a design and uses a proper WebSocket interface. Specifically:

● Design a JSON based API that  represents all messages and their replies (including errors)

● Re-write parts of your engine using WebSharper to implement the WebSocket interface

● Re-write parts of your client to use WebSockets.

● Implementes a public key-based authentication method for your interface. Specifically,

● A user, upon registration, provides a public key (can be RSA-2048 or a 256-bit ElipticCurve)

● When the user re-connects via WebSockets, it first authenticates using a challenge-based algorithm

● The engine sends a 256-bit challenge

● The client forms a message containing the challenge, the current time (UNIX time in seconds) and digitally signs it.

● The engine sends a confirmation or an error

● The engine is not allowed to reuse the challenge and it must only cache it for 1 second. If the protocol does not finish within 1s, it must be tried again

● The user establishes a secret key with the engine (using Diffie-Helman protocol) and HMAC signs every message sent

● The HMAC is computed over the serialized JSON content.
