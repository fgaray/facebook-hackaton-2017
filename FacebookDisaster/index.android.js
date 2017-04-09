/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 * @flow
 */

import ibrdtn from './ibrdtn';
import React, { Component } from 'react';
import {
  AppRegistry,
  StyleSheet,
  Text,
  View,
  Button,
} from 'react-native';


const onButtonPress = () =>{
    console.log("boton send");
    ibrdtn.send("dtn://android-7e42bc4.dtn");
}


export default class FacebookDisaster extends Component {
  constructor(props){
      super(props);
      ibrdtn.show('Awesome', ibrdtn.SHORT); 
      ibrdtn.init(); 
  }
  render() {
    return (
      <View style={styles.container}>
        <Text style={styles.welcome}>
          Welcome to React Native!
        </Text>
        <Text style={styles.instructions}>
          To get started, edit index.android.js
        </Text>
        <Text style={styles.instructions}>
          Double tap R on your keyboard to reload,{'\n'}
          Shake or press menu button for dev menu
        </Text>
        <Button onPress={onButtonPress} title="Send" color="#841584" accessibilityLabel="Learn more about this purple button" />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#F5FCFF',
  },
  welcome: {
    fontSize: 20,
    textAlign: 'center',
    margin: 10,
  },
  instructions: {
    textAlign: 'center',
    color: '#333333',
    marginBottom: 5,
  },
});

AppRegistry.registerComponent('FacebookDisaster', () => FacebookDisaster);
