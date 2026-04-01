(function () {
  "use strict";

  window.shinymvu = {};

  shinymvu.register = function () {
    document.addEventListener("alpine:init", function () {
      Alpine.data("module", function () {
        return {
          model: {},
          init: function () {
            var self = this;
            Shiny.addCustomMessageHandler("model_channel", function (data) {
              self.model = data;
            });
          },
          send: function (msg) {
            Shiny.setInputValue("counter-msg", msg, { priority: "event" })
          }
        };
      });
    });
  };

  shinymvu.register();
})();
