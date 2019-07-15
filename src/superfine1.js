var RECYCLED_NODE = 1
var TEXT_NODE = 3
var EMPTY_OBJ = {}
var EMPTY_ARR = []
var map = EMPTY_ARR.map
var isArray = Array.isArray

var merge = function(a, b) {
  var out = {}

  for (var k in a) out[k] = a[k]
  for (var k in b) out[k] = b[k]

  return out
}

var listener = function(event) {
  this.handlers[event.type](event)
}

var patchProperty = function(node, key, oldValue, newValue, dispatch, isSvg) {
  if (key === "key") {
  } else if (key[0] === "o" && key[1] === "n") {
    if (
      // node call : custom
      !((node.handlers || (node.handlers = {}))[
        (key = key.slice(2).toLowerCase())
      ] = dispatch(newValue))
    ) {

      // node call
      node.removeEventListener(key, listener)
    } else if (!oldValue) {
      // node call
      node.addEventListener(key, listener)
    }
  } else if (!isSvg && key !== "list" && key in node) {
    // node call
    node[key] = newValue == null ? "" : newValue
  } else if (newValue == null || newValue === false) {
    // node call
    node.removeAttribute(key)
  } else {
    // node call
    node.setAttribute(key, newValue)
  }
}

var createNode = function(vnode, dispatch, isSvg) {
  var node =
    vnode.type === TEXT_NODE
      // node call
      ? document.createTextNode(vnode.name)
      : (isSvg = isSvg || vnode.name === "svg")
      ? document.createElementNS("http://www.w3.org/2000/svg", vnode.name)
      : document.createElement(vnode.name)
  var props = vnode.props

  for (var k in props) {
    patchProperty(node, k, null, props[k], dispatch, isSvg)
  }

  for (var i = 0, len = vnode.children.length; i < len; i++) {
    // node call
    node.appendChild(createNode(vnode.children[i], dispatch, isSvg))
  }

  // node call
  return (vnode.node = node)
}

var getKey = function(vnode) {
  return vnode == null ? null : vnode.key
}

var patchNode = function(parent, node, oldVNode, newVNode, dispatch, isSvg) {
  if (oldVNode === newVNode) {
  } else if (
    oldVNode != null &&
    oldVNode.type === TEXT_NODE &&
    newVNode.type === TEXT_NODE
  ) {
    // node call
    if (oldVNode.name !== newVNode.name) node.nodeValue = newVNode.name
  } else if (oldVNode == null || oldVNode.name !== newVNode.name) {
    // node call
    node = parent.insertBefore(createNode(newVNode, dispatch, isSvg), node)
    if (oldVNode != null) {
      // node call
      parent.removeChild(oldVNode.node)
    }
  } else {
    var tmpVKid
    var oldVKid

    var oldKey
    var newKey

    var oldVProps = oldVNode.props
    var newVProps = newVNode.props

    var oldVKids = oldVNode.children
    var newVKids = newVNode.children

    var oldHead = 0
    var newHead = 0
    var oldTail = oldVKids.length - 1
    var newTail = newVKids.length - 1

    isSvg = isSvg || newVNode.name === "svg"

    for (var i in merge(oldVProps, newVProps)) {
      if (
        (i === "value" || i === "selected" || i === "checked"
          ? node[i]
          : oldVProps[i]) !== newVProps[i]
      ) {
        patchProperty(node, i, oldVProps[i], newVProps[i], dispatch, isSvg)
      }
    }

    while (newHead <= newTail && oldHead <= oldTail) {
      if (
        (oldKey = getKey(oldVKids[oldHead])) == null ||
        oldKey !== getKey(newVKids[newHead])
      ) {
        break
      }

      patchNode(
        node,
        oldVKids[oldHead].node,
        oldVKids[oldHead++],
        newVKids[newHead++],
        dispatch,
        isSvg
      )
    }

    while (newHead <= newTail && oldHead <= oldTail) {
      if (
        (oldKey = getKey(oldVKids[oldTail])) == null ||
        oldKey !== getKey(newVKids[newTail])
      ) {
        break
      }

      patchNode(
        node,
        oldVKids[oldTail].node,
        oldVKids[oldTail--],
        newVKids[newTail--],
        dispatch,
        isSvg
      )
    }

    if (oldHead > oldTail) {
      while (newHead <= newTail) {
        // node call
        node.insertBefore(
          createNode(newVKids[newHead++], dispatch, isSvg),
          (oldVKid = oldVKids[oldHead]) && oldVKid.node
        )
      }
    } else if (newHead > newTail) {
      while (oldHead <= oldTail) {
        // node call
        node.removeChild(oldVKids[oldHead++].node)
      }
    } else {
      for (var i = oldHead, keyed = {}, newKeyed = {}; i <= oldTail; i++) {
        if ((oldKey = oldVKids[i].key) != null) {
          keyed[oldKey] = oldVKids[i]
        }
      }

      while (newHead <= newTail) {
        oldKey = getKey((oldVKid = oldVKids[oldHead]))
        newKey = getKey(newVKids[newHead])

        if (
          newKeyed[oldKey] ||
          (newKey != null && newKey === getKey(oldVKids[oldHead + 1]))
        ) {
          if (oldKey == null) {
            // node call
            node.removeChild(oldVKid.node)
          }
          oldHead++
          continue
        }

        if (newKey == null || oldVNode.type === RECYCLED_NODE) {
          if (oldKey == null) {
            patchNode(
              node,
              oldVKid && oldVKid.node,
              oldVKid,
              newVKids[newHead],
              dispatch,
              isSvg
            )
            newHead++
          }
          oldHead++
        } else {
          if (oldKey === newKey) {
            patchNode(node, oldVKid.node, oldVKid, newVKids[newHead], dispatch, isSvg)
            newKeyed[newKey] = true
            oldHead++
          } else {
            if ((tmpVKid = keyed[newKey]) != null) {
              patchNode(
                node,
                // node call
                node.insertBefore(tmpVKid.node, oldVKid && oldVKid.node),
                tmpVKid,
                newVKids[newHead],
                dispatch,
                isSvg
              )
              newKeyed[newKey] = true
            } else {
              patchNode(
                node,
                oldVKid && oldVKid.node,
                null,
                newVKids[newHead],
                dispatch,
                isSvg
              )
            }
          }
          newHead++
        }
      }

      while (oldHead <= oldTail) {
        if (getKey((oldVKid = oldVKids[oldHead++])) == null) {
          // node call
          node.removeChild(oldVKid.node)
        }
      }

      for (var i in keyed) {
        if (newKeyed[i] == null) {
          // node call
          node.removeChild(keyed[i].node)
        }
      }
    }
  }

  // node call
  return (newVNode.node = node)
}

var createVNode = function(name, props, children, node, key, type) {
  return {
    name: name,
    props: props,
    children: children,
    node: node,
    type: type,
    key: key
  }
}

var createTextVNode = function(value, node) {
  return createVNode(value, EMPTY_OBJ, EMPTY_ARR, node, null, TEXT_NODE)
}

var recycleNode = function(node) {
  return node.nodeType === TEXT_NODE
    // node call
    ? createTextVNode(node.nodeValue, node)
    : createVNode(
        // node call
        node.nodeName.toLowerCase(),
        EMPTY_OBJ,
        // node call
        map.call(node.childNodes, recycleNode),
        node,
        null,
        RECYCLED_NODE
      )
}

export var patch = function(node, vdom, dispatch) {
  // node call
  node = patchNode(node.parentNode, node, node.vdom || recycleNode(node), vdom, dispatch)
  node.vdom = vdom
  // return node
}

var unwrapBsProps = function (props) {
  return props.reduce(
    function(newProps, prop) {
      if (prop) {
        newProps[prop[0][0]] = prop[0][1]
      }
      return newProps
    },
    {}
  )
}

export var h = function(name, props) {
  for (var vnode, rest = [], children = [], i = arguments.length; i-- > 2; ) {
    rest.push(arguments[i])
  }

  while (rest.length > 0) {
    if (isArray((vnode = rest.pop()))) {
      for (var i = vnode.length; i-- > 0; ) {
        rest.push(vnode[i])
      }
    } else if (vnode === false || vnode === true || vnode == null) {
    } else {
      children.push(typeof vnode === "object" ? vnode : createTextVNode(vnode))
    }
  }

  props = props ? unwrapBsProps(props) : EMPTY_OBJ

  return typeof name === "function"
    ? name(props, children)
    : createVNode(name, props, children, null, props.key)
}
