var nodeCall = function(fn, args, node) {
  fn.apply(node, args)
  return node
}

var nodeGet = function(key, node) {
  return node[key]
}

var nodeSet = function(key, val, node) {
  node[key] = val
  return node
}

export var setNodeValue = function(str, node) {
  return nodeSet('nodeValue', str, node)
}

export var setAttribute = function(key, val, node) {
  return nodeCall(node.setAttribute, [key, val], node)
}

export var removeAttribute = function(key, node) {
  return nodeCall(node.removeAttribute, [key], node)
}

export var insertBefore = function(newNode, refNode, parent) {
  return nodeCall(parent.insertBefore, [newNode, refNode], parent)
}

export var removeChild = function(node, parent) {
  return nodeCall(parent.removeChild, [node], parent)
}

export var parentNode = function(node) {
  return nodeGet('parentNode', node)
}

export var childNodeAt = function(index, parent) {
  return nodeGet('childNodes', parent)[index] || null
}

export var getVdom = function (node) {
  return nodeGet('vdom', node)
}

export var setVdom = function(vdom, node) {
  return nodeSet('vdom', vdom, node)
}

export var listener = function(event) {
  // TODO: check if currentTarget === this every time, otherwise there could be unexpected bugs.
  var handler = event.currentTarget.handlers[event.type]
  handler[0](handler[1], event)
}

export var deleteKey = function(dict, key) {
  delete dict[key]
  return 0
}

export var setEventHandler = function(key, handler, node) {
  (node.handlers || (node.handlers = {}))[key] = handler
  handler ? node.addEventListener(key, listener) : node.removeEventListener(key, listener)
  return node
}
