---
outline: [2, 4]
---
# Release Notes

See [Deprecations](/resources/deprecations) for what is superseded but still
shipping, and for the full removal list with migration notes.

## 1.143.0
2026-08-16
- Added `z2ui5_cl_ui5_view_builder`, the view builder this documentation is written against: `factory( )` / `ele( )` / `tag( )` / `a( )` / `end( )` / `stringify( )`, with every UI5 control and property reachable because the builder knows none of them by name. `z2ui5_cl_xml_view` is frozen, not removed — it ships unchanged and keeps working
- The model is pushed **automatically** when an event roundtrip changed it. The framework compares the model before and after `main( )` and sends it to every open view slot when it differs, so a handler can no longer render stale by forgetting a call. An unchanged model sends no payload at all
- Error output: a 500 now carries the full diagnostic — the message chain, one block per exception (class, text, source position, kernel error id, public attributes) and the app / event / draft / url / system context. The error popup shows the messages only, with the detail one click away behind *Details* and in *Copy*
- The HTTP handler asks ICF to gzip its responses where the client accepts it — 70–85% less transfer on installations whose ICM profile does not compress already
- Added `_bind( json = abap_true )`: a bound string that already contains JSON is spliced into the model as a JSON node, for control properties that must receive an object (a `sap.ui.integration` Card manifest, whose keys are not valid ABAP field names)
- Control-valued event arguments now reach the backend as data — `ViewSettingsDialog.confirm`, `SinglePlanningCalendar.selectedDatesChange` and the like used to break serialization entirely
- Added `setP13nData` to `CONTROL_METHODS`, so seeding a `sap.m.p13n` panel no longer needs hand-written JavaScript
- `message_toast_display( )` / `message_box_display( )` queue follow-up actions: several calls in one roundtrip all show, in call order, after the view rendered
- The startup page was rebuilt — the five quickstart steps top to bottom, then one row per sample repository with its install status, and the system information as a popup
- Developer Tools: the POPUP/POPOVER tabs now follow a dialog closed without a roundtrip; empty tabs grey out instead of opening blank; the NEST/NEST2 and System tabs are gone
- The wire got leaner throughout (internal): action lists travel as real JSON arrays, session-constant browser data travels once per page load, a display action implies the slot teardown, and the routing mode is only re-sent when the frontend may not hold it
- Fixes: a pending backend timer no longer fires into a destroyed controller; an app switch clears the previous app's keyboard shortcuts; a nested view re-displayed without its MAIN view gets the model too; `cc/Storage` compares by value, so a structure can be stored and read back; the `MessageToast` `Popup.Dock` warning is gone; the default CSP no longer names `frame-ancestors` in the `<meta>` tag
- `src/99` test classes are back in CI: all 27 restored, the two disabled `xml_view` suites re-enabled, and the freeze now covers production code only

**Obsolete — still compiles, still works**
- `view_model_update( )`, `popup_model_update( )`, `popover_model_update( )` and the two nested variants now do **nothing**. The push is automatic; delete the calls when you next touch the app
- `_event_client( )` → `follow_up_action( )`, which is the same call in the same position
- `_bind( custom_mapper = … custom_filter = … )` → `omit_initial` / `omit_initial_paths` / `json`, or shape it in ABAP
- `cs_event-wizard_set_next_step` → two `control_by_id` calls, which additionally reach `goToStep`

**Removed**
- `set_nav_back( )` and `set_nav_routing( )` from `z2ui5_if_client`. Routing is `follow_up_action( val = cs_event-set_nav_routing t_arg = ( mode ) )` now; `set_push_state( )` and `set_app_state_active( )` stay and delegate to the events
- `cs_event-nav_to_route` → `nav_app_call( )`, which is the real navigation and pushes the same route history entry when routing is on
- `cs_event-history_back` → `follow_up_action( |history.back()| )`, or `nav_app_leave( )` to return through the app stack
- `VIEWNAME` from `z2ui5_if_types=>ty_s_get`. The framework never filled it — an app reading `client->get( )-viewname` has to drop the read
- `z2ui5_if_app~check_sticky` / `check_initialized` moved to the framework's own app wrapper. Both attributes stay and are kept in sync, so a **read** still sees the truth; a direct **write** is no longer honored — use `set_session_stateful( )` and `check_on_init( )`
- Curated formatter: `round2DP`, `dimensions`, `stockStatusState`, `stockStatusIcon`, `deliveryStatusState`. Rounding, joining and status-to-`ValueState` mapping are things ABAP finishes — bind the result. The date helpers and `expandInlineIcons` remain
- `render_documentation( )`, `render_system_popup( )` and `render_contribution( )` from `z2ui5_cl_ui5_app_start`, with `cs_event-open_info`, `cs_event-close` and `cs_event-open_debug` — internals of the framework's own start page

## 1.142.0
2026-07-20
- Added frontend action functions: `control_by_id`, `binding_call`
- Added new frontend events, e.g. `SYSTEM_LOGOUT` and `KEYBOARD_SET_MODE`
- Extended the Debug Tool with error/log tabs and export (renamed to Developer Tools)
- Performance: introduced delta data transfer — only changed fields/rows are sent to the backend
- Compatibility: UI5 1.71 / UI5 2.x fixes (lazy module loading, lifecycle, aggregation escaping)
- Security: error message sanitization, security response headers, fatal-error overlay
- Samples repository fully reworked
- Various small improvements and bug fixes

**Removed**
- `z2ui5_cl_util_api` and its environment-specific variants `z2ui5_cl_util_api_c` / `z2ui5_cl_util_api_s`. The methods were kept: `bal_*`, `tr_*`, `conv_get_itab_by_xlsx` / `conv_get_xlsx_by_itab` and `source_get_method` are now on `z2ui5_cl_util_ext`, the remaining ones on `z2ui5_cl_util`. Replace the `z2ui5_cl_util_api=>` prefix with the class that now holds the method
- `z2ui5_cl_pop_bal` (BAL message popup) — removed without a replacement. The other built-in popups were not removed; they moved into the frozen package and keep working unchanged

## 1.141.0
2025-12-14
- Added Image Editor popup for image manipulation
- Added Camera Selector control with facing mode support
- Added Camera Picture control with configurable height/width
- Implemented security headers in HTTP response handling
- Added experimental `check_on_event` method for improved event handling *(since adopted as a fully supported dispatch API alongside `CASE client->get( )-event`)*
- Added experimental `_event_nav_app_leave` method for navigation
- Improved error handling with enhanced error popup
- Updated ajson library to latest version
- Code quality improvements: refactoring, formatting, and ABAP Cloud ATC findings
- Various bug fixes and performance improvements

## 1.140.0
2025-09-15
- New User Exit Logic with greater customization options
- Added Data Binding with References
- Optimized Data Binding Logic for faster performance
- ABAP Cloud Compatibility Adjustments
- Bug fixes for Data Loss Protection
- Bug fixes for Stateful Functionality
- Various bug fixes and performance improvements

## 1.139.0
2025-06-26
- Improved documentation
- Added multiple new control properties
- Fixed Launchpad compatibility issues
- Various bug fixes and performance improvements

## 1.138.0
2025-03-31
- Introduced a Copy to Clipboard feature for improved usability
- Enhanced the BAL message popup with multiple improvements
- Resolved serialization issues for numeric values
- Fixed Launchpad compatibility problems with older UI5 versions
- Updated the bundled ajson library to the latest version

## 1.137.0
2025-01-26
- Introduced App State feature
- Added App State Persistence for Navigation
- Fixed long string bugs for older releases
- Resolved popup get value issues
- Ensured Launchpad compatibility with OpenUI5 2.x
- Multiple bug fixes and new properties added

## 1.136.0
2024-12-15
- Extended Model Handling with OData Support
- UI5 2.x Compatibility
- New Documentation
- Navigation Container in Popover
- New UI Controls: AvatarGroup, Viz Charts, and more
- SetMaxWidth for Launchpads
- Multiple bug fixes and new properties added

## 1.135.0
2024-10-27
- Stateful Session Handling
- Object List Item now supports "type" attribute
- Message Strip with formatted text options
- Customizable Icons for Links, Expandable Text with class attributes
- Improved Frontend File Organization
- Various bug fixes for Launchpad compatibility

## 1.134.0
2024-09-22
- Added Tile Controls
- Added Color Control
- Added Multiselect Feature for Popup to Select
- Added Data Loss Feature
- Separated Custom Controls & Layouts from abap2UI5 Core
- Multiple bug fixes and new properties added

## 1.133.0
2024-08-24
- Added Controls & Properties for Splitter, Grid, Generic Tag, Content Areas, Step Input, News Content, Numeric Content, etc.
- Added Shell Bar Properties & Events
- Added Side Navigation Properties & Events
- Added Color Picker Properties
- Updated Layout Management of Table Popups
- Fixed Low Release Compatibility of Method to Read Fix Values
- Multiple bug fixes and new properties added

## 1.132.0
2024-07-28
- Added Properties for Multi Input, Slider, Search Field, etc.
- Added Date Range Selection Control, etc.
- Fixed Generic Tile & Avatar Control
- Fixed Binding with Custom Mapper
- Fixed Downport Functionality
- Fixed ABAP 750 Syntax
- Multiple bug fixes and new properties added

## 1.131.0
2024-07-03
- Added Radio Button, Radio Button Group, Date Range Selector
- Added Popup Layout V2
- Fixed Launchpad Title
- Fixed Syntax for 7.50

## 1.130.0
2024-06-13
- Added Wizard Control, Wizard Steps
- Added Frontend Info CC with Device Information
- Fixed Binding Logic, bind_clear replaced
- Fixed Multiple Issues for Focus CC
- Added Message Strip Visible Property

## 1.129.0
2024-06-01
- Added New Attributes for FilterGroupItem Control
- Added New Attributes for VariantManagement Control
- Fixed Issues with Data Model & Binding Logic
- Addressed Fixes for SAP Fiori Launchpad Integration

## 1.128.0
2024-05-20
- Fixed Popover and Message Toast Adjustments
- Fixed Launchpad Compatibility
- Fixed Data Binding with Generic Data References
- Added New Properties for the XML View

## 1.127.0
2024-05-05
- Added New Properties for Range Slider and Status Indicator
- Added Message Toast & Message Box Improvements
- Added Support for Automatic Renaming
- Bug fixes and small improvements

## 1.126.0
2024-04-17
- Fixed XML View Extensions
- Improved Support for Attributes created with RTTI
- Updated Spreadsheet CC for OpenUI5 Compatibility

## 1.125.0
2024-04-06
- Fixed Binding for Attributes created with RTTI
- Added XML View Extensions
- Replaced Magic Numbers and Fixed Auto Renaming
- Added Download base64 Files
- Added Features for Interactive Charts

## 1.124.0
2024-04-01
- Data Binding Improvements
- Fixed Popup Handling
- Fixed App in App Handling
- Added New XML Properties for MultiInput and Table
- Added Table with Column Menu

## 1.123.0
2024-03-20
- Added Card Control
- Fixed Nested View Data Binding
- Fixed Boolean Type Conversion
- Added Popover in Popup
- Added New Control Properties

## 1.122.0
2024-03-11
- Added New Control Properties
- Improved F4-Dialog with Description
- Fixed JSON Handling for Type P
- Fixed Launchpad Compatibility for BTP and Title Handling

## 1.121.0
2024-03-01
- Fixed Launchpad Compatibility
- Added Custom Control Message Manager
- Fixed Issues for low UI5 releases

## 1.120.0
2024-02-25
- Added Templating
- Added Popup & Layout Functionality
- Fixed Debugging Tools XML Output
- Fixed Messaging

## 1.119.0
2024-02-16
- Added Source Code View for Debugging Tools
- Added New Parameters for the UI5 Tree Control
- Changed Design for Start Page

## 1.118.0
2024-02-08
- Replaced /ui2/cl_json with ajson
- Fixed Compatibility for Low Releases

## 1.117.0
2024-02-02
- Added NavContainer in Popups
- Added New Properties to XML View
- Improved POPUP_TO_SELECT (title, sorting, descriptions)
- Fixed Issues for ABAP for Cloud Readiness

## 1.116.0
2024-01-26
- Added StartUp App with Value Help
- Added Popup Error, Popup Range, Popup Table
- Added Custom Control Chart.js
- Fixed Popup Model Handling
- Bug fixes

## 1.115.0
2024-01-19
- Fixed Binding Logic
- Added Popups (Confirm, Inform, Select Entry, PDF View, Message View)

## 1.114.0
2024-01-12
- Fixed Cross App Navigation
- Fixed Table Output
- Changed Debugger Tools

## 1.113.0
2024-01-03
- Added New Debugging Tools
- Changed UI5 Module Loading
