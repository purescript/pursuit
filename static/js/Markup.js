
window.Markup = (function() {
"use strict"
var exports = {}

function looksLikeAttributes(obj) {
  return typeof obj === 'object' && !looksLikeHtmlElement(obj)
}

function looksLikeHtmlElement(obj) {
  return obj && obj.nodeType !== undefined
}

exports.tag = function tag() {
  var args = Array.prototype.slice.call(arguments)
  var tagName = args.shift()
  var el = document.createElement(tagName)

  if (looksLikeAttributes(args[0])) {
    var attrs = args.shift()
    for (var prop in attrs) {
      if (attrs.hasOwnProperty(prop)) {
        el.setAttribute(prop, attrs[prop])
      }
    }
  }

  while (args.length > 0) {
    var child = args.shift()
    if (looksLikeHtmlElement(child)) {
      el.appendChild(child)
    } else if (typeof child === 'string') {
      var textNode = document.createTextNode(child)
      el.appendChild(textNode)
    } else if (child === null) {
      // no-op
    } else {
      throw new Error(
          'Markup.tag: Don\'t know what to do with this object')
    }
  }

  return el
}

// Partially apply a function of two or more arguments.
function partial(fn, arg) {
  return function() {
    return fn.apply(fn, [arg].concat(Array.prototype.slice.call(arguments)))
  }
}

var tagNames = ['p', 'a', 'code', 'strong']

tagNames.forEach(function(tagName) {
  exports[tagName] = partial(exports.tag, tagName)
})

return exports
})()

