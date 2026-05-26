---
outline: [2, 4]
---
# Info

abap2UI5 ships the current frontend state with every roundtrip. Read it from `client->get( )` — no custom control, no extra event needed. The relevant sub-structures are `s_device`, `s_ui5`, `s_focus`, and `s_scroll`.

#### Device

`client->get( )-s_device` describes the user's device and browser: operating system, browser name and version, screen orientation, current viewport size, and capability flags (touch, pointer, retina).

```abap
DATA(device) = client->get( )-s_device.

DATA(system)      = device-system.            " e.g. `desktop`, `phone`, `tablet`
DATA(orientation) = device-orientation.       " `landscape` | `portrait`

DATA(browser)     = device-browser-name.      " e.g. `chrome`
DATA(brw_version) = device-browser-version.

DATA(os)          = device-os-name.           " e.g. `win`, `mac`, `ios`, `android`
DATA(os_version)  = device-os-version.

DATA(width)       = device-resize-width.      " current viewport, in px
DATA(height)      = device-resize-height.

DATA(touch)       = device-support-touch.
DATA(pointer)     = device-support-pointer.
DATA(retina)      = device-support-retina.
```

::: tip **Bind Directly in the View**
If you only need device information in the view (not in ABAP logic), bind to the built-in UI5 device model via `{device>/...}` instead — no backend roundtrip required. See [Device Model](../model/device_model.md).
:::

#### UI5

`client->get( )-s_ui5` returns the runtime UI5 framework details — handy for version-dependent branching or for logging which build a user runs on.

```abap
DATA(ui5) = client->get( )-s_ui5.

DATA(version)         = ui5-version.           " e.g. `1.141.0`
DATA(build_timestamp) = ui5-build_timestamp.
DATA(gav)             = ui5-gav.               " group, artifact, version
DATA(theme)           = ui5-theme.             " e.g. `sap_horizon`
```

#### Focus

`client->get( )-s_focus` tells you which control currently holds the focus and where the caret sits inside it. Useful when an action depends on the field the user was just editing.

```abap
DATA(focus) = client->get( )-s_focus.

DATA(id)              = focus-id.                " id of the focused control
DATA(selection_start) = focus-selection_start.   " caret start, in chars
DATA(selection_end)   = focus-selection_end.     " caret end, in chars
```

To move the focus from the backend instead of reading it, see [Focus](../browser_interaction/focus.md).

#### Scroll

`client->get( )-s_scroll` reports the scroll positions of the page and any open dialogs at the moment the event was fired. Each container exposes the id of the scrollable element and its `x` / `y` offsets in pixels.

```abap
DATA(scroll) = client->get( )-s_scroll.

DATA(main_y)    = scroll-main-y.       " main page
DATA(nest_y)    = scroll-nest-y.       " first nested view
DATA(nest2_y)   = scroll-nest2-y.      " second nested view
DATA(popup_y)   = scroll-popup-y.      " open popup
DATA(popover_y) = scroll-popover-y.    " open popover
```

To scroll from the backend, see [Scrolling](../browser_interaction/scrolling.md).
