package com.SocialDisaster.IBRDTN;

import android.content.Intent;
import android.util.Log;
import android.widget.Toast;

import com.SocialDisaster.MainActivity;
import com.facebook.react.bridge.NativeModule;
import com.facebook.react.bridge.ReactApplicationContext; 
import com.facebook.react.bridge.ReactContext; 
import com.facebook.react.bridge.ReactContextBaseJavaModule; 
import com.facebook.react.bridge.ReactMethod;

import com.SocialDisaster.IBRDTN.DTNService;
import com.facebook.react.shell.MainPackageConfig;

import java.util.HashMap;
import java.util.Map;

import static android.R.id.message;

public class BindingIBRDTN extends ReactContextBaseJavaModule {

    private static final String TAG =  "BindingDTN";
    private DTNService service;

    private static final String DURATION_SHORT_KEY = "SHORT";

    public BindingIBRDTN(ReactApplicationContext reactContext) { 
        super(reactContext);
        Log.d(TAG, "Oncrete");
    }

    @Override
    public String getName() {
        return "BindingIBRDTN";
    }
    @Override
    public Map<String, Object> getConstants() {
        final Map<String, Object> constants = new HashMap<>();
        constants.put(DURATION_SHORT_KEY, Toast.LENGTH_SHORT);
        return constants;
    }

    // para probar si funciona el modulo
    @ReactMethod
    public void show(String message, int duration) {
        Toast.makeText(getReactApplicationContext(), message, duration).show();
    }


    @ReactMethod
    public void init() {
        service = new DTNService();
    }

    @ReactMethod
    public void send(String dir) {
        Log.d(TAG, "SEND: " + dir);
        final Intent intent = new Intent(getReactApplicationContext(), DTNService.class);
        intent.setAction(DTNService.ACTION_SEND_MESSAGE);
        intent.putExtra(DTNService.EXTRA_ID_USER, dir);
        intent.putExtra(DTNService.EXTRA_TEXT_USER, "HOLA");
        getReactApplicationContext().startService(intent);
    }
}

