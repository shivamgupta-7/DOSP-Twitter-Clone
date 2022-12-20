# DOSP-Twitter-Clone
Developed as Part of Coursework for COP5615 - Distributed Operating Systems

https://user-images.githubusercontent.com/47007121/208736670-26130ac5-d7ee-41c7-8668-6b31bf5c520c.mp4

In this project the aim is to implement a Twitter Clone and a client tester/simulator.


Implement a Twitter-like engine with the following functionality:

● Register account

● Send tweet. Tweets can have hashtags (e.g. #COP5615isgreat) and mentions (@bestuser)

● Subscribe to user's tweets

● Re-tweets (so that your subscribers get an interesting tweet you got by other means)

● Allow querying tweets subscribed to, tweets with specific hashtags, tweets in which the user is mentioned (my mentions)

● If the user is connected, deliver the above types of tweets live (without querying)

Implement a tester/simulator to test the above

● Simulate as many users as you can

● Simulate periods of live connection and disconnection for users

● Simulate a Zipf distribution on the number of subscribers. For accounts with a lot of subscribers, increase the number of tweets. Make some of these messages re-tweets

Other considerations:

● The client part (send/receive tweets) and the engine (distribute tweets) have to be in separate processes. Preferably, you use multiple independent client processes that simulate thousands of clients and a single-engine process

● Measurement of various aspects of your simulator and report performance 
