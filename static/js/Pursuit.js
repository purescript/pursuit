(function() {
/* Expects the version selector <select> element, which carries its per-page
 * configuration in data attributes:
 * - data-current-version (String):
 *    the version of docs that is being shown on this page.
 * - data-available-versions-url (String):
 *     The URL to fetch the available versions for this package from.
 *
 * This initializer is invoked from this (statically served) script rather than
 * from an inline per-page script, so that it keeps working across backend
 * restarts. Inline widget scripts are served from the backend's in-memory
 * EmbeddedStatic store and are orphaned when it restarts, which would leave
 * already-cached pages stuck showing "Loading …".
 */
function initializeVersionSelector(selector) {
  var currentVersion = selector.getAttribute('data-current-version')
  var availableVersionsUrl = selector.getAttribute('data-available-versions-url')

  function getJSON(url, callback) {
    var req = new XMLHttpRequest()
    req.open('GET', url, true)
    req.onload = function() {
      callback(JSON.parse(this.responseText))
    }
    req.send(null)
  }

  // Create a single <option> element, given a function to apply to the version
  // to display it, and a 2-element array of [version, url].
  function renderOption(fn, x) {
    var version = x[0]
    var url = x[1]

    var el = document.createElement("option")
    var text = document.createTextNode(fn(version))
    el.appendChild(text)

    el.setAttribute("value", url)

    // Set the 'selected' attribute on the current version
    if (version === currentVersion) {
      el.setAttribute('selected', null)
    }

    return el
  }

  // Returns an array of <option> elements which should be added to the version
  // selector. Warning: mutates the argument.
  function renderOptions(versions) {
    versions.reverse()
    return versions.map(function(x, index) {
      if (index === 0) {
        return renderOption(function(str) { return "latest (" + str + ")" }, x)
      } else {
        return renderOption(function(str) { return str }, x)
      }
    })
  }

  // Set an onchange handler so that selecting a version in the <select>
  // will navigate to the new page
  selector.onchange = function() {
    window.location.href = this.value
  }

  // Load the <option> elements via AJAX
  getJSON(availableVersionsUrl, function(data) {
    // Delete the placeholder <option>
    selector.removeChild(selector.firstChild)

    // Add the <option> elements that we got via AJAX to the <select>
    renderOptions(data).forEach(function(option) {
      selector.appendChild(option)
    })
  })
}

function initializeSearchForm() {
  var searchInput = document.getElementById('search-input')

  // Get the character that the given keyboard event refers to. Case-insensitive.
  function testChar(event, c) {
    var eventChar = String.fromCharCode(event.keyCode).toUpperCase()
    return eventChar == c.toUpperCase()
  }

  // Focus the search input on "S" keypress
  document.addEventListener('keydown', function(event) {
    if (document.activeElement.tagName === 'INPUT') {
      return
    }
    if (testChar(event, "S")) {
      event.preventDefault()
      document.getElementById('search-input').focus()
    }
  })


  // Change the placeholder for the search input based on whether it is focused.
  var normalPlaceholder = searchInput.getAttribute('data-normal-placeholder')
  var focusPlaceholder  = searchInput.getAttribute('data-focus-placeholder')

  searchInput.addEventListener('focus', function() {
    searchInput.setAttribute('placeholder', focusPlaceholder)
  })

  searchInput.addEventListener('blur', function() {
    searchInput.setAttribute('placeholder', normalPlaceholder)
  })

  if (document.activeElement === searchInput) {
    searchInput.setAttribute('placeholder', focusPlaceholder)
  }

  // Trim whitespace (for if there was whitespace in the query string)
  searchInput.value = searchInput.value.trim();

  // Render messages (if any)
  var message = Cookies.get('message')
  if (message) {
    var el = document.createElement('div')
    el.setAttribute('class', 'message success')
    var text = document.createTextNode(message)
    el.appendChild(text)
    document.getElementById('message-container').appendChild(el)
    Cookies.remove('message')
  }
}

function initializeLoadMoreLink(opts) {
  var link = document.getElementById('load-more-link')
  var results = document.getElementById('results-container')
  function makeOnClickHandler(url) {
    link.removeAttribute('data-load-more-url')
    return function() {
      var req = new XMLHttpRequest()
      req.open('GET', url, true)
      req.responseType = "document"
      req.onload = function() {
        var container = this.responseXML.getElementById('results-container')
        var children = container.childNodes
        var loadMoreUrl, status, msg, pEl
        for (var i = 0; i < children.length; i++) {
          results.appendChild(children[i])
        }
        if (loadMoreUrl = req.getResponseHeader("X-Load-More")) {
          link.onclick = makeOnClickHandler(loadMoreUrl)
        } else {
          status = req.getResponseHeader("X-No-More")
          switch (status) {
            case "limited":
              msg = "Further results have been omitted."
              break;
            case "exhausted":
              msg = "No further results."
              break;
            default:
              msg = "There are no more results, but the server did not appear to indicate why."
          }
          link.remove()
          loadMoreDiv = document.getElementById('load-more')
          pEl = document.createElement("p")
          pEl.appendChild(document.createTextNode(msg))
          loadMoreDiv.appendChild(pEl)
        }
      }
      req.send(null)
    }
  }
  var url = link.getAttribute('data-load-more-url')
  if (url) {
    link.onclick = makeOnClickHandler(url)
    link.href = 'javascript:void(0)'
  }
}

window.Pursuit = {
  initializeVersionSelector: initializeVersionSelector,
  initializeSearchForm: initializeSearchForm,
  initializeLoadMoreLink: initializeLoadMoreLink
}

// Wire everything up on page load. This script is served statically (embedded
// at compile time), so it is always available; doing initialization here rather
// than from inline per-page scripts means the page's dynamic behaviour survives
// backend restarts. This script is loaded in <head>, so wait for the DOM.
function onReady(fn) {
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', fn)
  } else {
    fn()
  }
}

onReady(function() {
  if (document.getElementById('search-input')) {
    initializeSearchForm()
  }

  var selectors = document.querySelectorAll('.version-selector')
  for (var i = 0; i < selectors.length; i++) {
    initializeVersionSelector(selectors[i])
  }

  if (document.getElementById('load-more-link')) {
    initializeLoadMoreLink()
  }
})
})()
