(function() {
  "use strict";

  window.shinymvu = window.shinymvu || {};

  shinymvu.register = function(config) {
    document.addEventListener("alpine:init", function() {
      Alpine.data(config.name, function() {
        var component = {
          model: {},
          init: function() {
            var self = this;
            Shiny.addCustomMessageHandler(config.modelChannel, function(data) {
              self.model = data;
            });
            if (config.handlers) {
              var channels = Object.keys(config.handlers);
              for (var i = 0; i < channels.length; i++) {
                Shiny.addCustomMessageHandler(
                  channels[i], config.handlers[channels[i]]
                );
              }
            }
          },
          send: function(type, value) {
            Shiny.setInputValue(
              config.msgId,
              { type: type, value: value === undefined ? null : value },
              { priority: "event" }
            );
          },
          shinySet: function(name) {
            Shiny.setInputValue(name, Date.now(), { priority: "event" });
          }
        };
        if (config.extend) {
          Object.assign(component, config.extend);
        }
        return component;
      });
    });
  };
})();
