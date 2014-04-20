/*jshint loopfunc: true */
function Display(canvas, log, tiles_wide, tiles_high, image_sources ) {
  this.canvas = canvas;
  this.log = log;
  this.tiles_wide = tiles_wide;
  this.tiles_tall = tiles_high;

  this.hit_canvas = document.createElement("canvas");
  this.hit_ctx = this.hit_canvas.getContext("2d");
  this.hit_canvas.width = 1; this.hit_canvas.height = 1;

  this.frameBuffer = document.createElement("canvas");
  this.frameBufferCtx = this.frameBuffer.getContext("2d");
  this.frameBuffer.width = this.tile_width * this.tiles_wide;
  this.frameBuffer.height = this.tile_height * this.tiles_tall;


  this.loadAssets( image_sources );
}

Display.prototype.canvas = undefined;
Display.prototype.image_banks = {};
Display.prototype.clock = 0;
Display.prototype.tiles_wide = 0;
Display.prototype.tiles_tall = 0;
Display.prototype.tile_width = 32;
Display.prototype.tile_height = 32;
Display.prototype.tile_data = {};
Display.prototype.world_pos = {x:0, y:0};
Display.prototype.frameBuffer = undefined;
Display.prototype.frameBufferCtx = undefined;
Display.prototype.mainCtx = undefined;
Display.prototype.hit_canvas = undefined;
Display.prototype.hit_ctx = undefined;
Display.prototype.log = undefined;

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

Display.prototype.hit_detect = function(mouse_evt) {
  var rect = this.canvas.getBoundingClientRect();
  var canvas_x = mouse_evt.clientX - Math.ceil(rect.left),
      canvas_y = mouse_evt.clientY - Math.ceil(rect.top);


  var tileCoords = this.canvas_coords_to_tile_coords( canvas_x, canvas_y );

  var sprites_hit = [];
  var sprites = this.GetSpritesForTile( tileCoords.x, tileCoords.y );
  for ( var z_index = 0; z_index < sprites.length; z_index++ ) {
    var sprite = sprites[z_index];
    var image_bank = this.image_banks[sprite.bank];
    var spriteDetails = this.get_sprite_image_bank_details( sprite );

    this.hit_ctx.clearRect(0,0,1,1);
    this.hit_ctx.drawImage( image_bank, spriteDetails.x + tileCoords.pxl_x, spriteDetails.y + tileCoords.pxl_y, 1, 1, 0, 0, 1, 1 );
    var test_pixel = this.hit_ctx.getImageData( 0, 0, 1, 1).data;
    if ( test_pixel[3] > 0 ) { //visible
      sprites_hit.push( sprite );
    }
  }

  if ( sprites_hit.length > 0 )
    return { tile_x: tileCoords.x, tile_y: tileCoords.y, object_id: sprites_hit.pop().object_id };
  else
    console.log( "Display.hit_detect: nothing was clicked?");

};

Display.prototype.loadAssets = function(sources) {
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

Display.prototype.draw_sprite_on_tile = function( sprite, tile_x, tile_y, ctx) {
  this.draw_sprite( sprite, this.tile_width * tile_x, this.tile_height * tile_y, ctx );
};

Display.prototype.draw_sprite = function( sprite, x, y, dest_ctx ) {
  var image_bank = this.image_banks[sprite.bank];

  var spriteDetails = this.get_sprite_image_bank_details( sprite );
  dest_ctx.drawImage( image_bank, spriteDetails.x, spriteDetails.y,
                      spriteDetails.width, spriteDetails.height,
                      x, y,
                      spriteDetails.width, spriteDetails.height );
};

Display.prototype.update_framebuffer = function() {
  var delta_x = (this.tiles_wide - 1) / 2,
      delta_y = (this.tiles_tall - 1) / 2;
  var player_pos = this.world_pos;

  var x_min = player_pos.x - delta_x, y_min = player_pos.y - delta_y,
      x_max = player_pos.x + delta_x, y_max = player_pos.y + delta_y;

  for ( var x = x_min, tile_x = 0; x <= x_max; x++, tile_x++ ) {
    for ( var y = y_min, tile_y = 0; y <= y_max; y++, tile_y++ ) {

      var sprites = this.GetSpritesForTile(x,y);
      if ( typeof sprites != 'undefined' ) {
        for ( var t = 0; t < sprites.length; t ++ ) {
          this.draw_sprite_on_tile( sprites[t], tile_x, tile_y, this.frameBufferCtx );
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

// INTERNAL

Display.prototype.get_sprite_image_bank_details = function ( sprite ) {
        var image_bank = this.image_banks[sprite.bank];
        var width = image_bank.meta.width,
            height = image_bank.meta.height;
        var framesWide = image_bank.width / width;
        var state_meta = image_bank.meta.states[sprite.state];
        var frameOffset = state_meta.frameOffset;
        var direction = Number(sprite.direction || 1);
        if (state_meta.clock_frames && "undefined" != typeof sprite.start) {
            var animOffset = state_meta.clock_frames[(this.clock - sprite.start) % state_meta.clock_frames.length];
            frameOffset += animOffset * state_meta.dirs;
        }
        frameOffset += direction - 1;
        var sx = frameOffset % framesWide * width,
            sy = Math.floor(frameOffset / framesWide) * height;
        return { x: sx, y: sy, width: width, height: height, image_bank: image_bank };
    };

Display.prototype.canvas_coords_to_tile_coords = function (canvas_x, canvas_y) {
      return {  x: Math.floor(canvas_x / this.tile_width) + this.world_pos.x - (this.tiles_wide - 1) / 2,
                y: Math.floor(canvas_y / this.tile_height) + this.world_pos.y - (this.tiles_tall - 1) / 2,
                pxl_x: canvas_x % this.tile_width,
                pxl_y: canvas_y % this.tile_height };
  };

Display.prototype.GetSpritesForTile = function(x, y) {
  return this.tile_data[x + ',' + y];
};