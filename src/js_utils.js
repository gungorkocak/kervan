export var listener = function(event) {
  var handler = event.currentTarget.handlers[event.type]
  handler[0](handler[1], event)
}

export var deleteKey = function(dict, key) {
  delete dict[key]
  return 0
}

export var setEventHandler = function(node, key, handler) {
  (node.handlers || (node.handlers = {}))[key] = handler
  handler ? node.addEventListener(key, listener) : node.removeEventListener(key, listener)
}
