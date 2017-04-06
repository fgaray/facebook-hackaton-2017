package com.hackerusach.cordovaibrdtn;

import org.apache.cordova.*;
import org.json.JSONArray; 
import org.json.JSONException;
import org.json.JSONObject; 


public class Ibrdtn extends CordovaPlugin {

    CordovaInterface mCordova;

    @Override
    public void initialize(CordovaInterface cordova, CordovaWebView webView) { 
        super.initialize(cordova, webView); mCordova = cordova; 
    }

    @Override 
    public boolean execute(String action, JSONArray args, CallbackContext callbackContext) throws JSONException{
        if (action.equals("echo")) {
            String message = args.getString(0);
            this.echo(message, callbackContext);
            return true;
        }
        return false; 
    } 

    private void echo(String message, CallbackContext callbackContext) {
        if (message != null && message.length() > 0) {
            callbackContext.success(message);
        } else {
            callbackContext.error("Expected one non-empty string argument.");
        }
    }
}

