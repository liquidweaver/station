function Log( log_div ) {
  this.log_div = log_div;
}

Log.prototype.log_div = undefined;

Log.prototype.log = function(text) {
  this.log_div.innerText += text + '\n';
};