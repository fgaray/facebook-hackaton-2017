package com.SocialDisaster.IBRDTN;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;


/**
 * Created by patricio on 08-04-17.
 */

public class EventReceiver extends BroadcastReceiver {

    private static String TAG = "EventReceiver";

    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        Log.d(TAG, "onReceive: " +  action);

        if (action.equals(de.tubs.ibr.dtn.Intent.RECEIVE))
        {
            // start receiving service
            Intent i = new Intent(context, DTNService.class);
            i.setAction(de.tubs.ibr.dtn.Intent.RECEIVE);
            context.startService(i);
        }
    }
}

