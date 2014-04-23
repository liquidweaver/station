function Protocol( display, log ) {
  this.log = log;
  this.display = display;
}

Protocol.prototype.display = {};
Protocol.prototype.server_socket = null;
Protocol.prototype.url = "ws://localhost:9001/";
Protocol.prototype.log = undefined;

Protocol.prototype.handler_need_login  = function( login_request, should_redraw  ) {
  var username = document.getElementById("username").value;

  this.send_message( 'username', username );
};

Protocol.prototype.handler_tile_data = function( tile_data, should_redraw  ) {
  for ( var coordinate in tile_data ) {
    this.display.tile_data[coordinate] = tile_data[coordinate];
  }
  console.log( 'tile_data recieved. ' + Object.keys(tile_data).length + ' tiles.');

  should_redraw = true;
};

Protocol.prototype.handler_world_pos = function( position, should_redraw ) {
  this.display.world_pos = position;

  console.log( 'world_pos updated: ' + position.x + ',' + position.y);

  should_redraw = true;
};

Protocol.prototype.handler_error = function( error_msg, should_redraw  ) {
  alert( error_msg );
};

Protocol.prototype.send_message = function( msg_type, data ) {
  var obj = {};
  if ( 'undefined' == typeof data) data = '';
  obj[msg_type] = data;
  this.server_socket.send( JSON.stringify(obj));
};

Protocol.prototype.connect_websocket = function( url ) {
  this.server_socket = new WebSocket( url || this.url );
  this.server_socket.onopen = function(){
    console.log('Connection open!');
  };
  this.server_socket.onclose = function(){
    console.log('Connection closed');
  };
  this.server_socket.onerror = function(error){
    console.log('Error detected: ' + error);
  };

  var _this = this; //For callback below
  this.server_socket.onmessage = function(e){
    var server_message = e.data;
    var should_redraw = false;
    msg = JSON.parse( e.data );

    for (var msg_type in msg) {
      if (_this['handler_' + msg_type])
        _this['handler_' + msg_type]( msg[msg_type], should_redraw );
      else
        console.log( "ERROR - Unknown msg_type: " + msg_type );
    }

    if ( should_redraw )
      display.redraw();
  };
};