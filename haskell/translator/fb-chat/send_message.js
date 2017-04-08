const fs = require("fs");
const login = require("facebook-chat-api");

var appState = process.argv[2];
var message = process.argv[3];
var threadId = process.argv[4];

login({appState: JSON.parse(appState)}, (err, api) => {
  api.sendMessage(message, threadId, function(err){
    console.log(err);
  });
});
