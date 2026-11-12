---
outline: [2, 4]
description: The map of the cookbook - one row per group, from the view and the model to EML and troubleshooting, and the cheat sheet to keep open while working.
---
# Cookbook

Concrete problems and their solutions, one chapter each, grouped by what an
app is made of. The [Cheat Sheet](/cookbook/cheat_sheet) is the checklist to
keep open while working; the groups below are where each rule on it is
explained in full. Every word the chapters rely on is defined in the
[Glossary](/get_started/glossary).

## I Want To…

| I want to… | Go to |
|---|---|
| build a first screen | [Definition → Sending a View](/cookbook/view/definition#sending-a-view) |
| find the right control | [Definition → Choosing a Control](/cookbook/view/definition#choosing-a-control) |
| show a value, or let the user edit it | [Binding](/cookbook/model/binding#editing-data) |
| format a date, a time or an amount | [Formatter](/cookbook/model/formatter#date) |
| show an internal table | [Tables → Basic Table](/cookbook/model/tables#basic-table) |
| know which table row was clicked | [Tables → Which Row Was Clicked](/cookbook/model/tables#which-row-was-clicked) |
| sort or filter a table | [Tables → Sorting and Filtering](/cookbook/model/tables#sorting-and-filtering) |
| show more than 100 rows | [More than 100 Rows](/cookbook/model/size_limit#set-the-limit) |
| react to a button | [Backend → Basic](/cookbook/event_navigation/backend#basic) |
| send a value with an event | [Backend → Sending a Value with the Event](/cookbook/event_navigation/backend#source) |
| do something in the browser without a roundtrip | [Frontend Actions](/cookbook/event_navigation/frontend#the-two-positions) |
| understand why my view is blank after a return | [App Structure](/cookbook/event_navigation/life_cycle#returning-from-a-sub-app-hits-check-on-navigated-not-check-on-init) |
| switch between two screens | [Inner App → Within One Class](/cookbook/event_navigation/navigation/inner_app#within-one-class) |
| call another app and get a result back | [Inner App → Handing a Result Back](/cookbook/event_navigation/navigation/inner_app#handing-a-result-back) |
| make the browser's Back button work | [URL Hash and Routing](/cookbook/event_navigation/navigation/hash#enabling-routing) |
| give the user a link to the current state | [App State](/cookbook/event_navigation/navigation/app_state#getting-the-link) |
| open and close a popup | [Popup](/cookbook/popup_popover/popup#open-and-close-a-popup) |
| ask a yes/no question | [Message → the parameters](/cookbook/translation_messages/message#the-parameters-of-message-box-display) |
| show a toast, an error or a BAPIRET table | [Message](/cookbook/translation_messages/message#sy-bapiret-cx-root) |
| handle an exception | [Errors and Exceptions](/cookbook/event_navigation/exception#handling-it-yourself) |
| set the focus | [Focus](/cookbook/browser_interaction/focus#set-the-focus) |
| refresh the screen every few seconds | [Timer](/cookbook/browser_interaction/timer#periodic-refresh) |
| upload or download a file | [Upload, Download](/cookbook/device_capabilities/upload_download#upload) |
| keep two users from overwriting each other | [Lock → Which Strategy?](/cookbook/expert_more/lock#which-strategy) |
| read and change a RAP business object | [EML](/cookbook/eml_cds_sql/eml) |
| find out why something does not work | [Common Failures](/cookbook/troubleshooting/common_failures) |

## The Groups

| Group | What it covers | Start with |
|---|---|---|
| View | Building the view in ABAP, nested views, XML templating | [Building a View](/cookbook/view/definition) |
| Model | Binding, expression binding and formatters, tables, trees, the device model, the size limit | [Binding](/cookbook/model/binding) |
| Event, Navigation | The life cycle, backend and frontend actions, navigation inside and between apps, the hash and the app state, exceptions | [App Structure](/cookbook/event_navigation/life_cycle) |
| Popup, Popover | Dialogs and popovers as fragments of their own | [Popup](/cookbook/popup_popover/popup) |
| Translation, Messages | Messages, logging, translation with i18n | [Message](/cookbook/translation_messages/message) |
| Browser Interaction | The title, focus, scrolling, timers, the clipboard, URL handling, the soft keyboard, keyboard shortcuts | [Title](/cookbook/browser_interaction/title) |
| Device Capabilities | What the frontend reports about itself, camera, geolocation, barcode scanning, audio and video, upload and download, PDF, spreadsheets | [Frontend Info](/cookbook/device_capabilities/info) |
| State, Connectivity | Locks, statefulness, WebSocket, OData, smart controls | [Lock](/cookbook/expert_more/lock) |
| EML, CDS, SQL | RAP, EML, RAP draft handling, CDS, ABAP SQL | [RAP](/cookbook/eml_cds_sql/rap) |
| Patterns, Helpers | Snippets, value help, demo output, e-mail, fuzzy search | [Snippets](/cookbook/expert_more/snippets) |
| Troubleshooting | The failures that come up most, and what each one means | [Common Failures](/cookbook/troubleshooting/common_failures) |
