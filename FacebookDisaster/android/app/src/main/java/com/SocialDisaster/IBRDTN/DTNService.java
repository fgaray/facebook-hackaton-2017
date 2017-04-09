package com.SocialDisaster.IBRDTN;

import android.content.Intent;
import android.util.Log;

import de.tubs.ibr.dtn.api.BundleID;
import de.tubs.ibr.dtn.api.DTNClient;
import de.tubs.ibr.dtn.api.DTNClient.Session;
import de.tubs.ibr.dtn.api.DTNIntentService;
import de.tubs.ibr.dtn.api.DataHandler;
import de.tubs.ibr.dtn.api.GroupEndpoint;
import de.tubs.ibr.dtn.api.Registration;
import de.tubs.ibr.dtn.api.ServiceNotAvailableException;
import de.tubs.ibr.dtn.api.SessionDestroyedException;
import de.tubs.ibr.dtn.api.SimpleDataHandler;
import de.tubs.ibr.dtn.api.SingletonEndpoint;
import de.tubs.ibr.dtn.api.;

/**
 * Created by patricio on 08-04-17.
 */

public class DTNService  extends DTNIntentService{
	private static final String TAG = "DTNService";
	public static final String EXTRA_ID_USER = "USER_ID" ;
	public static final String EXTRA_TEXT_USER = "TEXT_ID" ;
	private DTNClient.Session mSession = null;



	// enviar mensaje
	public static final String ACTION_SEND_MESSAGE = "com.SocialDisaster.SEND_MESSAGE";
	// recibir mensaje
	public static final String ACTION_RECV_MESSAGE = "com.SocialDisaster.RECV_MESSAGE";


	private static final String ACTION_MARK_DELIVERED = "de.tubs.ibr.dtn.example.DELIVERED";
	private static final String EXTRA_BUNDLEID = "de.tubs.ibr.dtn.example.BUNDLEID";

	public static final String EXTRA_SOURCE = "de.tubs.ibr.dtn.example.SOURCE";
	public static final String EXTRA_DESTINATION = "de.tubs.ibr.dtn.example.DESTINATION";
	public static final String EXTRA_PAYLOAD = "de.tubs.ibr.dtn.example.PAYLOAD";
	public static final GroupEndpoint PRESENCE_GROUP = new GroupEndpoint("dtn://socialdisaster.dtn/presence");

	public DTNService() {
		super(TAG);
	}

    @Override 
    public void onCreate(){
		super.onCreate();
		Log.d(TAG, "Oncreate");
		Registration reg = new Registration("SocialDisaster");
		//reg.add(PRESENCE_GROUP);
		try {
			initialize(reg);
		} catch (ServiceNotAvailableException e) {
			Log.e(TAG, "Service not available", e);
		}
    }

	@Override
	protected void onSessionConnected(Session session) {
		Log.d(TAG, ">>>>> CONECTADO");
        mSession = session;
		mSession.setDataHandler(mDataHandler);
	}

	@Override
	protected void onSessionDisconnected() {
		Log.d(TAG, ">>>>> DESCONECTADO");
		mSession = null;
	}


    @Override
	protected void onHandleIntent(Intent intent) {
		if(mSession == null) {
			return;
		}
		String action = intent.getAction();
		Log.d(TAG,"Action: " + action);

		if (de.tubs.ibr.dtn.Intent.RECEIVE.equals(action)) {
			Log.d(TAG,"MESSGE RECEIVE");
			try {
				//query all available bundles so it can be processed by the DataHandler
				while (mSession.queryNext()){
					Log.d(TAG, "query next");
				};
			} catch (SessionDestroyedException e) {
				Log.e(TAG, "session destroyed", e);
			}
		}else if(ACTION_SEND_MESSAGE.equals(action)) {
			Log.e(TAG,"SEND MESSAGE");
			String idUser = intent.getStringExtra(EXTRA_ID_USER);
			String text = intent.getStringExtra(EXTRA_TEXT_USER);
			actionSendMessage(idUser, text);
		}else if(ACTION_RECV_MESSAGE.equals(action)) {
			Log.d(TAG,"RECIEVE MESSAGE");
		}
	}

	private void actionSendMessage(String idUser, String text) {
		SingletonEndpoint endpoint = new SingletonEndpoint(idUser);
		try {
			mSession.send(endpoint, 3600, text.getBytes());
			Log.e(TAG, "Session SEND");
		} catch (SessionDestroyedException e) {
			e.printStackTrace();
			Log.e(TAG, " Session destroyer al enviar");
		}
	}


	/**
	 * Notice: The SimpleDataHandler only supports messages with
	 * a maximum size of 4096 bytes. Bundles of other sizes are
	 * not going to be delivered.
	 */
	private DataHandler mDataHandler = new SimpleDataHandler() {
		@Override
		protected void onMessage(BundleID id, byte[] data) {
			Log.d(TAG, "Message received from " + id.getSource());

			// forward message to an activity
			Intent mi = new Intent(ACTION_RECV_MESSAGE);
			mi.putExtra(EXTRA_SOURCE, id.getSource().toString());
			mi.putExtra(EXTRA_PAYLOAD, data);
			sendBroadcast(mi);

			// mark the bundle as delivered
			Intent i = new Intent(DTNService.this, DTNService.class);
			i.setAction(ACTION_MARK_DELIVERED);
			i.putExtra(EXTRA_BUNDLEID, id);
			startService(i);
		}
	};
}

