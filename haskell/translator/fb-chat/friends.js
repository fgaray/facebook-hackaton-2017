const fs = require("fs");
const login = require("facebook-chat-api");

var appState = process.argv[2];


login({appState: JSON.parse(appState)}, (err, api) => {
    if(err) return console.error(err);

    api.getFriendsList((err, data) => {
        if(err) return console.error(err);
        console.log(JSON.stringify(data));
    });
});
