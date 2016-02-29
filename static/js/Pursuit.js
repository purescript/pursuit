(function() {
/* Expects arguments as an Object with the following properties:
 * - currentVersion (String):
 *    the version of docs that is being shown on this page.
 * - elementId (String):
 *    the HTML id of the version selector element.
 * - availableVersionsUrl (String):
 *     The URL to fetch the available versions for this package from.
 */
function initializeVersionSelector(args) {
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
    if (version === args.currentVersion) {
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
  var selector = document.getElementById(args.elementId)
  selector.onchange = function() {
    window.location.href = this.value
  }

  // Load the <option> elements via AJAX
  getJSON(args.availableVersionsUrl, function(data) {
    // Delete the placeholder <option>
    selector.removeChild(selector.firstChild)

    // Add the <option> elements that we got via AJAX to the <select>
    renderOptions(data).forEach(function(option) {
      selector.appendChild(option)
    })
  })
}

/* Initializes client-side code to handle events concerning the uploading form
 * on /upload:
 * - On change, attempt to parse the file as JSON on the client side first
 * - If successful, try to extract the name and version of the package, and
 *   display those (to help reassure the user that they selected the right
 *   file)
 * - If unsuccessful, display a warning.
 */
function initializeUploadForm() {
  function notUndefined(x) {
    if (x === undefined) {
      throw new Error('notUndefined: got undefined')
    }
    return x
  }

  var statusElement = document.getElementById('upload-form-status')

  function onInputChange(event) {
    var file = event.target.files[0]

    // remove previous error messages. Having one rendered by the server on the
    // page at the same time as one rendered client-side is a bit weird.
    var errors = document.querySelectorAll('div.message.error')
    Array.prototype.forEach.call(errors,
      function(el) {
        el.parentNode.removeChild(el)
      })

    var reader = new FileReader
    reader.onload = function(e) {
      var jsonString = e.target.result
      var info = showPackageInformation(jsonString)
      var el = info === null
                ? invalidUpload
                : info
      replaceChild(statusElement, el)
    }
    reader.readAsText(file)
  }

  document.querySelector('input[type="file"]')
    .addEventListener('change', onInputChange)

  function tryGetPackageNameAndVersion(jsonString) {
    try {
      var package     = JSON.parse(jsonString)
      var packageMeta = notUndefined(package.packageMeta)
      var name        = notUndefined(packageMeta.name)
      var version     = notUndefined(package.version)
      return { name: name, version: version }
    } catch(e) {
      return null
    }
  }

  var M = window.Markup

  // String                  -- ^ encoded JSON
  // -> Nullable HtmlElement -- ^ HTML to be displayed. Null means not JSON
  //                         --   or missing required properties.
  function showPackageInformation(str) {
    var pkg = tryGetPackageNameAndVersion(str)
    if (pkg === null) {
      return null;
    }

    return M.p({class: 'message success'},
      "You're uploading ", M.strong(pkg.name), " at version ",
      M.strong(pkg.version), ".")
  }

  var invalidUpload = M.p({class: 'message error'},
      "The file you selected could not be parsed. Make sure you select a file",
      " which was produced by ", M.code("psc-publish"), ". See the ",
      M.a({href: '/help#submitting-packages'}, "package uploading guide"),
      " for details.")

  function replaceChild(el, child) {
    while (el.firstChild) {
      el.removeChild(el.firstChild)
    }
    el.appendChild(child)
  }
}

function initializeSearchForm() {
  var searchInput = document.getElementById('search-input')

  // Get the character that the given keyboard event refers to. Case-insensitive.
  function testChar(event, c) {
    var eventChar = String.fromCharCode(event.keyCode).toUpperCase()
    return eventChar == c.toUpperCase()
  }

  // Focus the search input on "S" or "/" keypresses
  document.addEventListener('keydown', function(event) {
    if (document.activeElement.tagName === 'INPUT') {
      return
    }
    if (testChar(event, 'S') || testChar(event, '/')) {
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

window.Pursuit = {
  initializeVersionSelector: initializeVersionSelector,
  initializeUploadForm: initializeUploadForm,
  initializeSearchForm: initializeSearchForm,
}
})()
