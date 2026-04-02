(function () {
  "use strict";

  window.shinymvu = {};

  shinymvu.register = function (config) {
    document.addEventListener("alpine:init", function () {
      Alpine.data(config.component, function () {
        return {
          model: {},
          init: function () {
            var self = this;
            Shiny.addCustomMessageHandler(
              config.model_channel,
              function (data) { self.model = data; }
            );
          },
          send: function (msg) {
            Shiny.setInputValue(config.msg_id, msg, { priority: "event" })
          }
        };
      });
    });
  };
})();
