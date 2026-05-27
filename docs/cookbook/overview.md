---
outline: [2, 4]
---
# Overview

The Cookbook collects task-oriented recipes for everyday abap2UI5 development. Each section focuses on one area of the framework and shows the patterns you reach for most often. Use this page as a map — pick the topic that matches the problem in front of you and jump straight in.

### Sections

#### [View](/cookbook/view/definition)
Build the XML view your app sends to the browser. Covers the basic view definition, nesting views inside other views, and XML templating for repeating structures.

#### [Model](/cookbook/model/binding)
Share data between ABAP and the frontend. Explains one-way and two-way binding, expressions and formatters, tables and trees, the device model, and how to deal with the model size limit.

#### [Event, Navigation](/cookbook/event_navigation/life_cycle)
Understand how a request flows through your app. Covers the lifecycle, backend and frontend events, actions, navigation between apps, and exception handling.

#### [Popup, Popover](/cookbook/popup_popover/popup)
Overlay parts of the view with dialogs and popovers — custom popups, popovers anchored to a control, and the built-in dialogs the framework ships out of the box.

#### [Translation, Messages](/cookbook/translation_messages/message)
Communicate with the user. Show message toasts and message boxes, write to the application log, and translate text with `i18n`.

#### [Browser Interaction](/cookbook/browser_interaction/title)
Reach into the browser from ABAP — set the tab title, control focus and scrolling, run timers, access the clipboard, work with the URL, and handle the soft keyboard on mobile.

#### [Device Capabilities](/cookbook/device_capabilities/info)
Read device info and use native hardware: camera, geolocation, barcode scanning, audio, and file upload/download including PDF and spreadsheet generation.

#### [State, Connectivity](/cookbook/expert_more/lock)
Manage session and app state across requests and talk to the outside — locking, statefulness, WebSocket, logout, OData, and sharing app state via URL.

#### [More Topics](/cookbook/eml_cds_sql/rap)
EML/CDS/SQL integration with RAP, recurring patterns and helpers, troubleshooting, and a list of obsolete features kept for reference.

### How to Use This Cookbook

- Each recipe stands on its own — read only the section you need.
- Code snippets are copy-paste ready. Drop them into a class that implements `z2ui5_if_app`.
- For full sample apps, browse the 250+ examples in the [samples repository](https://github.com/abap2UI5/samples).
- For the API surface (`z2ui5_if_client`, `z2ui5_cl_xml_view`, …), read the source in the [main repository](https://github.com/abap2UI5/abap2UI5).

→ New to abap2UI5? Start with the [Getting Started Guide](/get_started/quickstart).
