package com.SocialDisaster.IBRDTN;

import android.widget.Toast;

import com.facebook.react.bridge.NativeModule;
import com.facebook.react.bridge.ReactApplicationContext; 
import com.facebook.react.bridge.ReactContext; 
import com.facebook.react.bridge.ReactContextBaseJavaModule; 
import com.facebook.react.bridge.ReactMethod;

import java.util.HashMap;
import java.util.Map;

public class BindingIBRDTN extends ReactContextBaseJavaModule {

    private static final String DURATION_SHORT_KEY = "SHORT";

    public BindingIBRDTN(ReactApplicationContext reactContext) { 
        super(reactContext);
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

    @ReactMethod
    public void show(String message, int duration) {
        Toast.makeText(getReactApplicationContext(), message, duration).show();
    }
}

