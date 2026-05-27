---
outline: [2, 4]
---
# Device Model

abap2UI5 offers two ways to access device information: directly in the view via the UI5 device model (frontend), or in ABAP logic via `client->get( )-s_device` (backend).

### Frontend

By default, the device model binds to the view under the name `device`. Use standard UI5 binding syntax to show device properties directly — no backend roundtrip needed:
```abap
page->input(
  description = `device model - resize - width`
  value       = `{device>/resize/width}`  ).
```
For all parameters, see the [UI5 docs](https://sapui5.hana.ondemand.com/sdk/#/api/sap.ui.Device).

### Backend
When you need device information in your ABAP logic (e.g., to adapt behavior based on the browser or screen size), read it from `client->get( )-s_device` — no custom control, no extra event needed. The value is shipped with every roundtrip:

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
