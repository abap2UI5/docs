---
outline: [2, 4]
description: Debugging an abap2UI5 app - the external breakpoint for an HTTP request, what the 500 body carries and how check_hide_error_details changes it, the error id that links a user's report to the monitor, the built-in developer tools, and the ICM and ICF traces.
---
# Debugging
Since all logic runs in ABAP, you can debug everything in the ABAP environment. Set an external breakpoint, because abap2UI5 apps are called externally via HTTP.

## Backend

### External breakpoint
A session breakpoint stops only the SAP GUI session it was set in, and an abap2UI5 roundtrip runs in a work process the ICF picked, under the user who sent the request. So set an **external** breakpoint, for that user:

- In ADT: *Run → Toggle Breakpoint* on a line of your app class with the breakpoint type *External* (the default in ADT), after setting the user in *Window → Preferences → ABAP Development → Debug* when the request runs under another user than yours — for a technical user behind a connector or a destination, for instance.
- In SAP GUI: in the editor, *Utilities → Settings → ABAP Editor → Debugging* holds the user the external breakpoints are for, and the breakpoint button in SE24 / SE80 sets external breakpoints with *Breakpoints → External Breakpoint*. `SRDEBUG` activates external debugging for a user without a breakpoint in the code.

Then fire the event in the browser: the debugger opens on the next roundtrip that reaches the line. Put the breakpoint in `z2ui5_if_app~main` of your class, or on the event branch you are after; a breakpoint in the framework is rarely needed, since everything an app does runs inside its own `main( )`.

### The 500 and what it carries
An exception that leaves `main( )` ends the roundtrip with HTTP 500, and the body of that response is the only diagnostic the developer gets: the browser shows it in the fatal-error overlay, and nothing of it survives the roundtrip anywhere else unless a [monitor](/configuration/monitoring) is on. The body is the **full** exception chain — every previous exception with its class, source position and attributes, not only the outermost message — plus a short context block with the system id, the release and the time, and an **error id** stamped on it (`id : …`). Host name, client and user are deliberately left out of it, because the body reaches the browser.

With `check_hide_error_details = abap_true` in the user exit's `set_config_http_post` (see [User Exits](/advanced/extensibility/user_exits)) the body is replaced by `Internal Server Error` — the hardening the [Security](/configuration/security) page describes for productive systems. The id is then the only thing a user can report, and it is what connects the report to the roundtrip monitor: `error_text` of the monitor's roundtrip structure is the same rendering of the chain, with the same id, so the full chain is on the server even when the browser got nothing. The Admin Cockpit's Errors tab groups by it; a monitor of your own gets it in `is_roundtrip-error_text`. On a development system leave the details visible, and on a productive one switch the monitor on before you hide them, or an error is gone with the response.

Check that the XML view builds correctly and that all events fire on the backend as expected.

## Frontend
On the frontend, abap2UI5 behaves like a standard UI5 app, so the usual tools and debugging features work.

### Developer Tools
Press `Ctrl+F12` to open the built-in **Developer Tools** of abap2UI5:
![Developer Tools showing XML View and Data Model inspection](/configuration/debug.webp)
Six tabs cover the whole roundtrip: **Overview**, **Problems**, **Roundtrips** (each request and its response), **View & Data** (the rendered XML and the model of the slot you pick — main, popup, popover, nested), **System** and **Search**.

The footer offers **Retry**, **Restart**, **Logout**, a jump to **ADT**, **Copy**, **Report a Bug** and an **Export** that bundles everything — including the running app's ABAP class source — into one blob you can attach to a bug report. Error popups also carry a copy-to-clipboard button for the same purpose.

### UI5 Inspector
Another option: the SAP default debugging tool, the [UI5 Inspector](https://chromewebstore.google.com/detail/ui5-inspector/bebecogbafbighhaildooiibipcnbngo).

## When the request does not reach the app
A request that fails before `main( )` runs — a 401, a 403 from the [CSRF check](/configuration/security#cross-site-request-forgery-csrf) or from the node's authorization, a 404 on the service path, a timeout — leaves no 500 body to read. Two standard traces say what happened to it:

- **`SICF` → the node → *Edit → Trace*** records every request of that node with its headers and its status; the *Error Pages* tab of the node decides what a logon or an authorization failure shows instead of your app.
- **`SMICM` → menu *Goto → Trace File*** (trace level 2 or higher for one reproduction, back to 1 afterwards) shows the request on the ICM level — the host it came in on, the path, whether a web dispatcher or a proxy rewrote it, which is where a wrong `Origin` or a wrong forwarded host shows up. The HTTP cache behind a stale frontend is cleared there too (menu *Goto → HTTP Plug-In → Server Cache → Invalidate*).

The browser's network tab beside them shows the request as the frontend sent it: the status, the body of the response and, for a 500, the same text the overlay shows.

## Issues
If your code looks correct but you suspect a bug in the abap2UI5 framework, build a minimal sample that reproduces the bug and open an issue on the abap2UI5 repository — we'll investigate and fix it.
