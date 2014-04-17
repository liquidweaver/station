function Log( textarea ) {
  this.textarea = textarea;
}

Log.prototype.textarea = undefined;

Log.prototype.log = function(text) {
  this.textarea.value += text + '\n';
};