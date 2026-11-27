---
outline: [2, 3]
description: What changes for abap2UI5 users and add-on users with 1.151.0 - the actions to take when you update, the new API, the add-ons and what moved.
---
# What Changed in November 2026

In November 2026 the abap2UI5 repositories were reorganized, and the work is
released as 1.151.0. Two things make it matter for every installation: one
abapGit pull of abap2UI5 is now a running installation, with the ICF node, its
handler and the launchpad BSP included, and the legacy objects the core no
longer ships live in one place,
[abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext). On top of that
come a declarative timer, new navigation helpers, a test harness for apps,
smaller drafts and a faster first page load - and new features in the popups,
layout-management, table-maintenance and custom-controls add-ons. The
[Release Notes](/resources/changelog#_1-151-0) list the release, 1.151.0 of
2026-11-25; this page is the overview for the update.

## Do This When You Update

Breaking changes and required actions first. Most installations need only the
first three items.

- [ ] **Pull abap2UI5 alone.** The ICF node `/sap/bc/z2ui5`, its handler and
  the BSP `Z2UI5` for the launchpad are part of abap2UI5 now (package
  `src/03`). Node and BSP keep their names, so target mappings and tiles stay
  as they are. The handler is renamed: `z2ui5_cl_lp_handler` is
  `z2ui5_cl_ui5_icf_handler`. See
  [What a Pull Brings](/configuration/installation#frontend-branches).
- [ ] **Retire the frontend repository.** abap2UI5/frontend is retired. If an
  abapGit repository on your system points at one of its branches (`standard`,
  `standard_new`, `cloud`, ...), remove it from abapGit **without deleting
  its objects**, then pull abap2UI5. The old handler `Z2UI5_CL_LP_HANDLER` is
  named by nothing afterwards and can be deleted. See
  [Fiori Launchpad](/configuration/installation#frontend-branches).
- [ ] **ABAP Cloud: pull the branch `cloud`.** It is generated from `main` on
  every push and carries the HTTP service, its handler, the IAM objects and the
  app as a Fiori project in `app/`. It replaces the branch `cloud_new` of the
  frontend repository. See [S/4 Public Cloud](/configuration/s4_public_cloud).
- [ ] **Still using legacy objects? Install abap2UI5-ext.** The old view
  builder, the built-in popups, the old types interface, the utility classes,
  their exception class and their table are no longer part of the core. They
  ship, unchanged and under the same names, from
  [abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) (`main`, or
  `702` on NetWeaver 7.02 - 7.4x). If you installed them from the frontend
  repository between 2026-11-17 and 2026-11-25, change the URL of that
  abapGit repository to abap2UI5-ext and pull. Do not confirm the deletion
  of the utilities' table - it and its data stay in the system. The full list
  is on [Deprecations](/resources/deprecations#version-status).
- [ ] **Exit class: implement `z2ui5_if_ui5_exit`.** The former name of the
  exit interface is deleted and no longer looked up. An exit class still
  implementing it does not compile, and the installation runs on the shipped
  defaults (no own Content-Security-Policy, theme or bootstrap) until it
  implements `z2ui5_if_ui5_exit` with the same two methods.
- [ ] **ICF node or own handler: name `z2ui5_cl_ui5_http_handler`.** The
  former name of the HTTP entry point (an empty subclass) is deleted.
- [ ] **Replace `_bind_edit( )` with `_bind( )`** (same parameters, without
  `custom_mapper_back` / `custom_filter_back`) and **delete every
  `view_model_update( )`**, `nest_view_model_update( )`,
  `nest2_view_model_update( )`, `popup_model_update( )` and
  `popover_model_update( )` call. The methods are removed; the model is sent
  whenever `main( )` changed it. The [linter](/advanced/linter) fixes both
  with `--fix`.
- [ ] **Windowed tables are app-driven.** `window_bind_tab( val total event
  window )`: the app loads, sorts, filters and counts the rows itself, and
  the browser's requests arrive as the app's event with
  `z2ui5_if_client=>ty_s_window_request` in `get( )-r_event_data`
  (`cs_window_reason` MORE / SORT / FILTER). `t_sort`, `t_filter` and
  `window_get_row( )` are removed. See
  [Windowed Tables](/cookbook/model/windowed_tables#coming-from-1-150-0).
- [ ] **Offline queue removed.** The experimental offline queue of 1.149.0
  and 1.150.0 is gone; delete the call. An event that cannot reach the backend
  ends in the error dialog with *Retry*.
- [ ] **Hash writes need a listener.** `hash_set( )` and `hash_replace( )`
  (experimental) write the hash only for an app that owns it - one that
  registered the hash listener (`hash_attach_changed`) with hash routing off -
  and the value is the whole app hash. Register the listener in your
  `view_display( )`.
- [ ] **Own draft store?** The draft store interface
  `z2ui5_if_ui5_draft_store` has two new methods, `prune_chain( )` and
  `touch( )`; an installation with its own store implements both.
- [ ] **JavaScript outside a view that loads a custom control?** The custom
  controls (`z2ui5.cc`) are a bundle of their own, loaded before the first
  view that names one. JavaScript outside a view that requires
  `z2ui5/cc/<Name>` on a page whose views name no control has to load the
  bundle first. Apps change nothing.
- [ ] **Own frontend or a tool that reads the wire?** The protocol is version
  3: the response describes the screen in the sections VIEWS, NAV and ACTIONS,
  a start request carries the route as `S_FRONT.ROUTE`, and the flags of an
  event travel as one object behind the event name. Apps change nothing;
  `_event( )` writes the new form. See
  [abap2UI5/protocol](https://github.com/abap2UI5/protocol).
- [ ] **Add-ons: update them together.** Pull layout-management together with
  popups, and adapt calls of the layout table builder (see
  [layout-management](#layout-management) below).

## New in abap2UI5

### Navigation and Popups

- `check_on_nav_back( )` is true on the one roundtrip on which the app gets
  the screen back from an app it called - a popup app closing included. It
  replaces comparing the class name of `get_app_prev( )`.
- `nav_app_call( tag = ... )` and `get_nav_tag( )`: the caller names why it
  calls and reads that name when the called app comes back, so one popup
  class can serve several purposes without a flag attribute per purpose.
- `check_nav_pending( )` says whether this roundtrip already ends in a
  navigation, for a base class that must not arm a timer or clear messages
  for a screen it is about to give away.
- `nav_app_leave( keep_messages = abap_true )` and `nav_app_call(
  keep_messages = abap_true )` show the message toasts and boxes queued in
  this roundtrip after the target app has rendered, instead of dropping them.
  A carried message box keeps its close event, which the target app receives.

### Timers

- `timer_set( event delay_ms no_busy )` and `timer_stop( )` declare a timer
  per app. The framework arms it whenever the app gets the screen - first
  display, the return from a called app or popup, a Back/Forward restore - and
  stops it when the app is left. An app may state it on every roundtrip; it
  no longer re-arms a poll in its `check_on_navigated( )` branch. See
  [Timer](/cookbook/browser_interaction/timer).
- The frontend event `START_TIMER` takes a fourth argument, the repeat flag:
  the browser then arms the same tick again after every response, and a user
  event in between pauses it. The new event `STOP_TIMER` ends it.
- A timer tick that leaves the app unchanged writes no draft, a click that
  meets a tick in flight is sent after the tick instead of being dropped, and
  the error dialog's *Retry* keeps a repeating timer.

### Launchpad and Title

- The `set_title` event sets the launchpad shell title as well when the app
  runs in the Fiori launchpad, so one call titles the screen in both places.
  A title that arrives before the launchpad's shell service is ready is set
  once it is there.
- `set_title_launchpad` no longer overwrites the shell title of a host app:
  an app embedded in a page it does not own sets none and writes a line to
  the log.
- `get_start_param( name )` reads one start parameter, whichever way the app
  was started: the URL query, the launchpad's startup parameters, or the
  query of the launchpad intent in the hash. See
  [Fiori Launchpad](/configuration/launchpad#read-startup-parameters).

### Drafts

- A draft stores only the rows of the binding table it needs, and no type
  descriptors: a large app writes 38 of its 436 rows, its draft 2.18 MB where
  it was 2.40 MB.
- The new user exit field `draft_keep_per_chain` of
  `ty_s_http_config_post` (2 by default, 0 switches it off) keeps the last
  drafts of a chain of roundtrips and deletes older ones on every save.
  Drafts of the app stack, of an app state link and of a route restore are
  never deleted.

### Testing

- `z2ui5_cl_ui5_test` is a public test harness for apps: a `FOR TESTING`
  class that drives a `z2ui5_if_app` class from ABAP Unit through the real
  framework, one roundtrip per call, without a test double of the client.
  `start( )`, `set_model( )`, `event( )`, `timer( )`, `leave( )` and
  `restore( )` act; `view_xml( )`, `popup_xml( )`, `model( )`, `messages( )`
  and `nav_called( )` read what the app shows. Drafts stay in memory. Runs on
  the transpiled runtime, NetWeaver 7.02 and ABAP Cloud.

### Binding and the View Builder

- An attribute an interface declares binds - `_bind( zif_state~mv_value )`,
  and through a reference typed with the interface. It raised
  `BINDING_ERROR` before.
- Fields and table columns with a namespace (`/ABC/FIELD`) bind and render:
  a namespace is a level of its row in the model, so a row template binds
  the column relatively as `{ABC/FIELD}`.
- `z2ui5_cl_ui5_view_builder=>component_path( name )` returns that relative
  path, for a row template over columns known only at run time.
- `message_box_display( icon = ... )` takes the icon of the box again.

### Speed and Delivery

- The page the node serves is a small UI5 app around the embed control, and the
  frontend is a cached bundle of its own, delivered without comments and
  whitespace (about 250 KB instead of 354 KB). Once cached, a start costs no
  request for the frontend.
- The bundle and `sap.m` load while UI5 boots: the first view is on screen
  about 0.5 s earlier.
- The start app tells a class that cannot be loaded (a syntax error, a
  missing dependency) from one that does not exist, with the reason.
- `npm run express:live` serves the frontend straight from its sources for
  frontend work, without a rebuild.

## Add-ons

### popups

[abap2UI5-addons/popups](https://github.com/abap2UI5-addons/popups)

- Every popup implements `z2ui5_if_popup_result`: read the result the same
  way for each popup with `check_confirmed( )`, `value( )` and
  `value_string( )`.
- New popups: a message popover with a count per type
  (`z2ui5_cl_popup_msgpopover`), a calculator (`z2ui5_cl_popup_calculator`),
  and developer text searches for data elements and messages
  (`z2ui5_cl_popup_dtel_srch`, `z2ui5_cl_popup_msg_search`) that also run
  embedded in an app's own view through `z2ui5_if_popup_embed`.
- The search help popup `z2ui5_cl_popup_shlp` opens a search help by name,
  with fields filled in advance, and expands collective search helps.
- Library methods that write, such as `add_data_to_tranport( )`, take
  `iv_commit`: with `abap_false` the caller commits, in one LUW with its own
  changes.
- `z2ui5_cl_popup_sel_layout` is removed - use `z2ui5_cl_layo_pop_w_sel` of
  layout-management. Pull layout-management together with popups.

### layout-management

[abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management)

- **Breaking:** `xml_build_table( )` and `xml_build_simple_form( )` of
  `z2ui5_cl_layo_xml_builder` take and write into a node of
  `z2ui5_cl_ui5_view_builder`. The add-on needs nothing but the abap2UI5
  core.
- `xml_build_table( )` has new options: sorting by a click on the column
  header, a row color, cell controls (icon, tag, switch, input, button, ...),
  several controls in one cell, and a table without toolbar.
- `z2ui5_cl_layo_pop_w_sel` selects rows from any table in a popup with
  saved column layouts.
- `copy_layouts( )` copies saved layouts to other handles, and
  `select_layouts_by_range( )` reads layouts by ranges, also of every user.
- Without an implementation of `z2ui5_if_layo_auth` every user may save
  shared layouts again.
- ALPHA and NUMC columns are formatted in the browser.

### table-maintenance

[abap2UI5-addons/table-maintenance](https://github.com/abap2UI5-addons/table-maintenance)

- A factory, `z2ui5_cl_tm_001=>factory( )`, with a read-only mode and a key
  of its own for the saved column layouts.
- The view cluster `z2ui5_cl_tm_002` maintains several tables on one page,
  with one transport request for all of them.
- The table is locked while it is edited (Standard ABAP), and a save never
  overwrites rows someone else changed since they were read.
- Save writes the database and the transport request in one LUW.

### custom-controls

[abap2UI5-addons/custom-controls](https://github.com/abap2UI5-addons/custom-controls)

- NetworkStatus (`z2ui5_cl_cci_net_status`): online, offline or server
  not reachable, with signal bars and latency, measured in the browser.
- Clock (`z2ui5_cl_cci_clock`): date and time ticking in the browser; it fits
  an overflow menu.
- ScanInput (`z2ui5_cl_cci_scan_input`): a scan field that keeps the focus -
  also inside a dialog - selects its content on focus, and beeps and
  vibrates on an error.

## Removed and Moved

| What | Where it is now | What to do |
|---|---|---|
| ICF node `/sap/bc/z2ui5` and BSP `Z2UI5` | abap2UI5, package `src/03` | pull abap2UI5 |
| ICF handler `z2ui5_cl_lp_handler` | renamed `z2ui5_cl_ui5_icf_handler` | delete the old class after the pull |
| custom-controls `z2ui5_cl_cci_rich_text_editor` | renamed `z2ui5_cl_cci_rich_text` (room for a namespace rename) | change the apps that call it, delete the old class after the pull |
| custom-controls `z2ui5_cl_cci_barcode_scanner` | renamed `z2ui5_cl_cci_barcode_scan` (room for a namespace rename) | change the apps that call it, delete the old class after the pull |
| custom-controls `z2ui5_cl_cci_network_status` | renamed `z2ui5_cl_cci_net_status` (room for a namespace rename) | change the apps that call it, delete the old class after the pull |
| custom-controls `z2ui5_cl_cci_signature_pad` | renamed `z2ui5_cl_cci_signature` (room for a namespace rename) | change the apps that call it, delete the old class after the pull |
| cloud HTTP service and app | abap2UI5, branch `cloud` | pull the branch `cloud` |
| abap2UI5/frontend | retired | remove it from abapGit without deleting its objects |
| old view builder, built-in popups, old types interface | abap2UI5-ext | install it next to abap2UI5 |
| utility classes, their exception class and table | abap2UI5-ext | install it next to abap2UI5 |
| former exit interface | deleted | implement `z2ui5_if_ui5_exit` |
| former HTTP handler name | deleted | name `z2ui5_cl_ui5_http_handler` |
| `_bind_edit( )` | removed | `_bind( )` |
| the five `*_model_update( )` methods | removed | delete the call |
| offline queue | removed | delete the call |
| `t_sort` / `t_filter` of `window_bind_tab( )`, `window_get_row( )` | removed | load, sort and filter on the window event |
| `z2ui5_cl_popup_sel_layout` (popups) | `z2ui5_cl_layo_pop_w_sel` (layout-management) | call the new class |

## Where to Read More

- [Deployment Scenarios](/configuration/installation#frontend-branches) - what
  a pull of `main`, `702` or `cloud` brings
- [Fiori Launchpad](/configuration/launchpad) - the BSP, coming from the
  frontend repository
- [S/4 Public Cloud](/configuration/s4_public_cloud) - the branch `cloud`
- [Versioning, Upgrade](/configuration/versioning) - how to upgrade and
  re-test
- [Deprecations](/resources/deprecations) - every superseded name with its
  successor
- [Release Notes](/resources/changelog) - every release
- [The changelog of the framework](https://github.com/abap2UI5/abap2UI5/blob/main/changelog.txt) -
  the same list at its source, section "v1.151.0"
- [abap2UI5-ext](https://github.com/abap2UI5/abap2UI5-ext) - the
  legacy objects and how to switch to the repository
