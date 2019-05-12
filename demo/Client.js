// findComments :: Effect (Array { key :: StaticPtr.Key, node :: Node })
exports.findComments = function() {
  var results = [];

  go(document.body);

  function go(node) {
    var PREFIX = 'client:';

    if(node.nodeType === Node.COMMENT_NODE) {
      var content = node.textContent.trim();
      if(content.indexOf(PREFIX) == 0) {
        results.push({ key: content.substring(PREFIX.length), node: node });
      }
      return;
    }
    for(var i = 0; i < node.childNodes.length; i++) {
      go(node.childNodes[i]);
    }
  }

  return results;
};

// clientImpl :: Node -> Effect Widget.Impl
//
//  { pushElement :: TagName -> Effect Unit
//  , popElement :: Effect Unit
//  , appendTextNode :: String -> Effect Unit
//  , client :: StaticPtr (Widget Unit) -> Effect Unit
//  }
exports.clientImpl = function(initialNode) {
  return function() {
    var nodesAfter = [initialNode];
    var parentNodes = [initialNode.parentNode];

    function appendNode(node) {
      var parentNode = parentNodes[0];
      var nodeAfter = nodesAfter[0];
      if(nodeAfter) {
        parentNode.insertBefore(node, nodeAfter);
      } else {
        parentNode.appendChild(node);
      }
    }

    return (
      { pushElement: function(tag) {
          return function() {
            var el = document.createElement(tag);
            appendNode(el);
            parentNodes.unshift(el);
            nodesAfter.unshift(null);
          };
        }

      , popElement: function() {
          parentNodes.shift();
          nodesAfter.shift();
        }

      , appendTextNode: function(text) {
          return function() {
            appendNode(document.createTextNode(text));
          };
        }
      , client: function() { return function() {}; }
    });
  };
};
