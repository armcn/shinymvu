(function () {
  "use strict";

  window.shinymvu = window.shinymvu || {};

  function registerHandlers(handlers) {
    Object.entries(handlers).forEach(function (entry) {
      Shiny.addCustomMessageHandler(entry[0], entry[1]);
    });
  }

  function createComponent(config) {
    var component = {
      model: {},
      init: function () {
        var self = this;
        Shiny.addCustomMessageHandler(config.modelChannel, function (data) {
          self.model = data;
        });
        if (config.handlers) registerHandlers(config.handlers);
      },
      send: function (type, value) {
        Shiny.setInputValue(
          config.msgId,
          { type: type, value: value === undefined ? null : value },
          { priority: "event" }
        );
      },
      shinySet: function (name) {
        Shiny.setInputValue(name, Date.now(), { priority: "event" });
      }
    };
    return config.extend ? Object.assign(component, config.extend) : component;
  }

  shinymvu.register = function (config) {
    document.addEventListener("alpine:init", function () {
      Alpine.data(config.name, function () {
        return createComponent(config);
      });
    });
  };
})();
