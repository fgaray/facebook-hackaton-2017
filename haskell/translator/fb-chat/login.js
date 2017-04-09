const fs = require("fs");
const login = require("facebook-chat-api");

var user = process.argv[2];
var passwd = process.argv[3];

login({email: user, password: passwd}, (err, api) => {
  if(err) return console.error(err);
   console.log(JSON.stringify(api.getAppState()));
});
