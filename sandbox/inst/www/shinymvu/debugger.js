(function() {
  "use strict";

  window.shinymvu = window.shinymvu || {};

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
