var express = require('express');
var app = express();
var expressWS = require('express-ws')(app);

var portNumber = 1234;


app.listen(portNumber, function() {
    console.log("server running on port: " + portNumber);
})

app.ws('/hello', function(websocket, request) {
    console.log('A client connected!');
  
    websocket.on('message', function(message) {
      console.log(`A client sent a message: ${message}`);
      websocket.send('Hello, client! Me the server, has received your message! Your are the man!');
    });
  });