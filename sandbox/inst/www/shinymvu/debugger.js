(function() {
  "use strict";

  window.shinymvu = window.shinymvu || {};

  shinymvu._modelState = {};

  shinymvu.toggleDebugger = function(panelId, tabId) {
    var panel = document.getElementById(panelId);
    var tab = document.getElementById(tabId);
    if (panel.style.display === "none" || panel.style.display === "") {
      panel.style.display = "flex";
      tab.style.display = "none";
    } else {
      panel.style.display = "none";
      tab.style.display = "flex";
    }
  };

  shinymvu.expandDebugger = function(panelId, btnId) {
    var panel = document.getElementById(panelId);
    var btn = document.getElementById(btnId);
    panel.classList.toggle("mvu-dbg-expanded");
    if (btn) {
      btn.textContent = panel.classList.contains("mvu-dbg-expanded") ? "Collapse" : "Expand";
    }
  };

  function nodeKey(details) {
    for (var i = 0; i < details.children.length; i++) {
      if (details.children[i].tagName === "SUMMARY") {
        var span = details.children[i].querySelector(".mvu-dbg-key");
        return span ? span.textContent : null;
      }
    }
    return null;
  }

  function nodePath(details, root) {
    var parts = [];
    var el = details;
    while (el && el !== root) {
      if (el.tagName === "DETAILS") {
        var k = nodeKey(el);
        if (k) parts.unshift(k);
      }
      el = el.parentElement;
    }
    return parts.join("/");
  }

  shinymvu.initModelPersistence = function(sectionId) {
    var section = document.getElementById(sectionId);
    if (!section || section._mvuObserving) return;
    section._mvuObserving = true;

    function restoreState() {
      var nodes = section.querySelectorAll("details.mvu-dbg-node");
      for (var i = 0; i < nodes.length; i++) {
        var path = nodePath(nodes[i], section);
        if (path in shinymvu._modelState) {
          nodes[i].open = shinymvu._modelState[path];
        } else if (path === "model") {
          nodes[i].open = true;
        }
      }
    }

    section.addEventListener("toggle", function(e) {
      if (e.target.tagName === "DETAILS") {
        var path = nodePath(e.target, section);
        if (path) shinymvu._modelState[path] = e.target.open;
      }
    }, true);

    var timer;
    var observer = new MutationObserver(function() {
      clearTimeout(timer);
      timer = setTimeout(restoreState, 10);
    });
    observer.observe(section, { childList: true, subtree: true });

    restoreState();
  };

  shinymvu.importSession = function(fileId, inputId) {
    var el = document.getElementById(fileId);
    el.onchange = function() {
      var file = el.files[0];
      if (!file) return;
      var reader = new FileReader();
      reader.onload = function(e) {
        Shiny.setInputValue(inputId, e.target.result, { priority: "event" });
      };
      reader.readAsText(file);
      el.value = "";
    };
    el.click();
  };
})();
