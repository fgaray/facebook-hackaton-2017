const fs = require("fs");
const login = require("facebook-chat-api");

var appState = process.argv[2];


login({appState: JSON.parse(appState)}, (err, api) => {
  api.getThreadList(0, 10, function(err, data){
    console.log(data);
  });
});
