# shinymvu 0.2.0

* Added `effect_cmd()` for async background tasks that dispatch results back
  through `update`, keeping the app interactive.
* Added `subscriptions` parameter to `mvu_module_server()` for consuming
  external reactive sources as messages into the MVU loop.
* Added `debug` parameter to `mvu_module_server()` for built-in time-travel
  debugging with model inspection, import/export, and step navigation.
* Simplified API to module-only: `mvu_module_ui()` and `mvu_module_server()`
  are the only public entry points.
* `debug` only needs to be set in `mvu_module_server()`, not in the UI.
* Added comprehensive guide vignette.

# shinymvu 0.1.0

* Initial release.
* Model-View-Update architecture for Shiny with Alpine.js rendering.
* `mvu_module_ui()` and `mvu_module_server()` for Shiny module integration.
* Immutable list helpers: `list_set()`, `list_get_in()`, `list_set_in()`,
  `list_update_in()`.
* Enum-based message types with `mvu_enum()` and exhaustive `match_enum()`.
* Effect system: `mvu_result()`, `effect_notify()`, `effect_send()`,
  `effect_custom()`.
* Input wrappers: `mvu_button()`, `mvu_text_input()`, `mvu_select_input()`,
  `mvu_slider_input()`, and others.
* Testing utility: `mvu_dispatch()` for headless unit testing of `update`.
