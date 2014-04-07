function Protocol( display ) {
  this.display = display;
}

Protocol.prototype.display = {};

Protocol.prototype.server_socket = null;
Protocol.prototype.url = "ws://localhost:9001/";

Protocol.prototype.handler_need_login  = function( login_request ) {
  var username = getElementById("username").value;

  this.server_socket.send_message("username", username);
}

Protocol.prototype.handler_tile_data = function( tile_data ) {
  for ( coordinate in tile_data ) {
    this.display.tile_data[coordinate] = tile_data[coordinate];
  }
  console.log( 'tile_data recieved. ' + Object.keys(tile_data).length + ' tiles.');
};

Protocol.prototype.send_message = function( web_socket, msg_type, data ) {
  this.server_socket.send( JSON.stringify({
    type: msg_type,
    data: data
  }));
};

Protocol.prototype.connect_websocket = function() {
  this.server_socket = new WebSocket(this.url);
  this.server_socket.onopen = function(){
    /*Send a small message to the console once the connection is established */
    console.log('Connection open!');
  }
  this.server_socket.onclose = function(){
    console.log('Connection closed');
  }
  this.server_socket.onerror = function(error){
    console.log('Error detected: ' + error);
  }

  var _this = this; //For callback below
  this.server_socket.onmessage = function(e){
    var server_message = e.data;
    msg = JSON.parse( e.data );

    for (msg_type in msg) {
      if (_this['handler_' + msg_type])
        _this['handler_' + msg_type]( msg[msg_type]);
      else
        console.log( "ERROR - Unknown msg_type: " + msg_type );
    }
  }
}