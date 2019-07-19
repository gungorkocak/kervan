export var listener = function(event) {
  event.currentTarget.handlers[event.type](event)
}

export var deleteKey = function(dict, key) {
  delete dict[key]
  return 0
}

export var setHandler = function(node, key, handler) {
  (node.handlers || (node.handlers = {}))[key] = handler
}
