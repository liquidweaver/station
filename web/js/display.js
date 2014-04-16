/*jshint loopfunc: true */
function Display(canvas, tiles_wide, tiles_high) {
  this.canvas = canvas;
  this.tile_width = tiles_wide;
  this.tile_height = tiles_high;

  this.frameBuffer = document.createElement("canvas");
  this.frameBufferCtx = this.frameBuffer.getContext("2d");
  this.frameBuffer.width = 32 * this.tile_width;
  this.frameBuffer.height = 32 * this.tile_height;

  this.loadAssets( sources, function(_this) {

  });
}

Display.prototype.canvas = undefined;
Display.prototype.image_banks = {};
Display.prototype.clock = 0;
Display.prototype.tile_width = 0;
Display.prototype.tile_height = 0;
Display.prototype.tile_data = {};
Display.prototype.world_pos = {x:0, y:0};
Display.prototype.frameBuffer = undefined;
Display.prototype.frameBufferCtx = undefined;
Display.prototype.mainCtx = undefined;

Display.prototype.draw_sprite_fb = function( sprite, x, y ) {
  this.draw_sprite( sprite, x, y, this.frameBufferCtx );
};

Display.prototype.update_canvas = function() {
  this.mainCtx.drawImage( this.frameBuffer, 0, 0 );
};

Display.prototype.tick = function() {
  this.clock++;
  this.redraw();
};

Display.prototype.redraw = function() {
  this.update_framebuffer();
  this.update_canvas();
};

Display.prototype.loadAssets = function(sources, callback) {
  var assetDir = 'sprites/';
  var imageExtension = '.dmi';
  var metaExtension = '.meta';
  var loadedAssets = 0;
  var numAssets = Object.keys(sources).length * 2;

  var _this = this; // For callback below
  loaded_callback = function() {
    if(++loadedAssets >= numAssets) {
      _this.canvas.width = _this.frameBuffer.width;
      _this.canvas.height = _this.frameBuffer.height;
      _this.mainCtx = _this.canvas.getContext("2d");

      console.log("Assets loaded.");
      setInterval(function(){_this.tick();}, 250);
    }
  };

  for(var src in sources) {
    this.image_banks[src] = new Image();
    this.image_banks[src].onload = loaded_callback;
    this.image_banks[src].src = assetDir + sources[src] + imageExtension;
    (function(bank, _this){
      $.get( assetDir + sources[src] + metaExtension, function( data ) {
        _this.image_banks[bank].meta = _this.parse_raw_dmi_meta( data );
        loaded_callback();
      });
    })(src, this);
  }
};

Display.prototype.draw_sprite = function( sprite, x, y, dest_ctx ) {
  var image_bank = this.image_banks[sprite.bank];
  var width = image_bank.meta.width,
  height = image_bank.meta.height;


  var framesWide = image_bank.width / width;
  var state_meta = image_bank.meta.states[sprite.state];
  var frameOffset = state_meta.frameOffset;
  var direction = Number(sprite.direction || 1);
  if ( state_meta.clock_frames && 'undefined' != typeof sprite.start ) {
    var animOffset = state_meta.clock_frames[ (this.clock - sprite.start) % state_meta.clock_frames.length ];
    frameOffset += animOffset * state_meta.dirs;
  }
  frameOffset += direction - 1;
  var sx = (frameOffset % framesWide) * width,
  sy = Math.floor(frameOffset / framesWide) * height;
  var px = x * 32, py = y * 32;

  dest_ctx.drawImage( image_bank, sx, sy, width, height, px, py, width, height );
};

Display.prototype.update_framebuffer = function() {
  var delta_x = (this.tile_width - 1) / 2,
  delta_y = (this.tile_height - 1) / 2;
  var player_pos = this.world_pos;

  var x_min = player_pos.x - delta_x, y_min = player_pos.y - delta_y;
      x_max = player_pos.x + delta_x, y_max = player_pos.y + delta_y;

  for ( var x = x_min; x < x_max; x++ ) {
    for ( var y = y_min; y < y_max; y++ ) {

      var tile = this.tile_data[ x + ',' + y];
      if ( typeof tile != 'undefined' ) {
        var abs_x = x - x_max, abs_y = y - y_max;
        
        for ( var t = 0; t < tile.length; t ++ ) {
          this.draw_sprite_fb( tile[t], abs_x, abs_y );
        }
      }
      
    }
  }
};

Display.prototype.clock_frame_map = function( delay_array ) {
  var clock_frames = [];
  var offset = 0, curFrameNum = 0;

  delay_array.forEach( function(lengthstr) {
    var length = Number(lengthstr);
    for ( x = offset; x < offset + length; x++ ) {
      clock_frames[x] = curFrameNum;
    }
    offset += length;
    curFrameNum++;
  });

  return clock_frames;
};

Display.prototype.parse_raw_dmi_meta = function( raw_dmi_meta ) {
  var reKeyVal = /(.+) = (.+)/g;
  var meta = {};
  var states = {};
  var curFrameOffset = 0;
  var curState;

  while ((kv = reKeyVal.exec(raw_dmi_meta)) !== null) {
    var key = kv[1], val = kv[2];
    if ( key == "version" ) {
      if ( val != "4.0" ) throw "Cannot decode DMI version" + val;
    }
    else if ( key == "state" ) {    // We are defining a new state
      if ( typeof curState != "undefined" ) {
        var dirs = states[curState].dirs  || 1;
        var frames = states[curState].frames || 1;
        curFrameOffset += dirs * frames;
      }
      curState = val.replace(/(^")|("$)/g, '');

      states[curState] = { frameOffset: curFrameOffset };
    }
    else {                          // Set a property of this state
      var prop = /\s+(.*)/.exec(key)[1];
      if (states[curState])
        states[curState][prop] = val;
      if ( prop == "delay")
        states[curState].clock_frames = this.clock_frame_map( val.split(',') );
      else                          //Set a global property for this image_bank
        meta[prop] = val;

    }
  }
  meta.states = states;
  return meta;
};