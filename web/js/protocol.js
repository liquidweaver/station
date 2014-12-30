function Protocol( display, log ) {
  this.log = log;
  this.display = display;
}

Protocol.prototype.display = {};
Protocol.prototype.server_socket = null;
Protocol.prototype.url = "ws://localhost:9001/";
Protocol.prototype.log = undefined;

Protocol.prototype.handle_need_login  = function( login_request, should_redraw  ) {
  var username = document.getElementById("username").value;

  this.send_message( 'username', username );
};

Protocol.prototype.handle_tile_data = function( tile_data, should_redraw  ) {
  for ( var coordinate in tile_data ) {
    this.display.tile_data[coordinate] = tile_data[coordinate];
  }
  if (Object.keys(tile_data).length > 1 )
    console.log( 'tile_data received, ' + Object.keys(tile_data).length + ' tiles.');
  else
    console.log( 'tile_data received: ' + JSON.stringify(tile_data) );

  should_redraw = true;
};

Protocol.prototype.handle_world_pos = function( position, should_redraw ) {
  this.display.world_pos = position;

  console.log( 'world_pos updated: ' + position.x + ',' + position.y);

  should_redraw = true;
};

Protocol.prototype.handle_error = function( error_msg, should_redraw  ) {
  this.log.log( error_msg );
};

Protocol.prototype.handle_timestamp = function( timestamp, should_redraw ) {
  this.display.clock = timestamp;
};

Protocol.prototype.handle_tick_length = function( tick_length, should_redraw ) {
  this.display.tick_length = tick_length;
};

Protocol.prototype.handle_message = function( message, should_redraw ) {
  this.log.log( message );
};

Protocol.prototype.send_message = function( msg_type, data ) {
  var obj = {};
  if ( 'undefined' == typeof data) data = '';
  obj[msg_type] = data;
  this.server_socket.send( JSON.stringify(obj));
};

Protocol.prototype.connect_websocket = function( url ) {
  if (this.server_socket) this.server_socket.close();
  this.server_socket = new WebSocket( url || this.url );
  var _this = this;
  this.server_socket.onopen = function(){
    _this.log.log('Connected to ' + this.url);
  };
  this.server_socket.onclose = function(){
    _this.log.log('Disconnected from ' + this.url);
  };
  this.server_socket.onerror = function(error){
    console.log('Error detected: ' + error);
  };

  this.server_socket.onmessage = function(e){
    var server_message = e.data;
    var should_redraw = false;
    msg = JSON.parse( e.data );

    for (var msg_type in msg) {
      if (_this['handle_' + msg_type])
        _this['handle_' + msg_type]( msg[msg_type], should_redraw ); //TODO: reduce over should_redraw
      else
        console.log( "ERROR - Unknown msg_type: " + msg_type );
    }

    if ( should_redraw )
      display.redraw();
  };
};