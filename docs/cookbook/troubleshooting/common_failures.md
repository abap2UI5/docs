---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_496
---
# Common Failures

Not every problem raises an ABAP exception. Many failures surface only in the browser, fail silently, or look like framework bugs when they are actually pattern mistakes. The sections below cover the ten most common ones — what the symptom looks like and where to find the real cause.

::: tip Developer Tools first
Press `Ctrl+F12` in the running app to open the built-in [Developer Tools](/configuration/debugging) — the **Error**, **Log**, **Previous Request** and **Response** tabs show most of what the browser-DevTools steps below dig for, without leaving the app.
:::

## Binding-Path Mismatch

When a `_bind` path does not resolve against the JSON model on the frontend — typically because the public attribute was renamed, the data was never sent, or the path is mistyped — UI5 does **not** raise an ABAP exception. The control simply renders empty or with a default value.

Where to look:
- **Browser console.** UI5 logs a warning like `Binding "/path/to/field" was not found in model` from `sap.ui.model.json.JSONModel`. Open the browser DevTools console and filter by `sap.ui.model`.
- **Network tab.** Inspect the abap2UI5 response payload — the JSON model is included verbatim. If the field is absent or named differently than your binding path, you have your answer.
- **ABAP side.** Nothing. The backend never learns the binding failed. Asserting "no console warnings" is the only way to catch this in tests.

Binding silently drops the write-back when the path is invalid — your ABAP attribute keeps its old value after the next event. Cross-check the attribute value in the debugger if data is mysteriously not updating.

## Bound Attribute Not Public

`_bind( )` resolves attributes via dynamic `ASSIGN` and only sees the `PUBLIC SECTION`. Anything declared `PROTECTED` or `PRIVATE` — or a local variable — cannot be found, and the framework says so rather than shrugging: it raises `z2ui5_cx_ui5_util_error` with

```
BINDING_ERROR - No class attribute for binding found -
Please check if the bound values are public attributes of your class
```

Nothing catches it on the way out, so the roundtrip answers 500 and the message lands in the error view. (A `_bind( val = … tab = … )` cell binding raises the same thing under `BINDING_ERROR_TAB_CELL_LEVEL`.)

Where to look:
- **The error text itself.** Unlike a binding-path mismatch, this failure is reported by name — if you are reading a browser-console warning, you are looking at the section above, not at this one.
- **Check the visibility of the attribute** in the class definition. Helper variables that never appear in a `_bind( )` call can stay private; anything you bind must move to `PUBLIC SECTION`.

See [Binding → Bound Attributes Must Be Public](/cookbook/model/binding).

## Type Coercion Without an Explicit UI5 Type

ABAP and UI5 do not share a type system. A `d` field goes on the wire as `YYYYMMDD`, but `DatePicker` expects an ISO date. `abap_bool` arrives as `"X"`/`""`, but `CheckBox` expects `true`/`false`. Packed numbers arrive as strings without locale formatting. The control renders the raw value, parses it wrong, or treats it as empty.

Where to look:
- **Symptom**: a `DatePicker` shows `Invalid Date` or refuses input; a `CheckBox` is always unchecked even when the attribute is `abap_true`; numeric inputs lose decimals or render with the wrong separator.
- **Fix**: attach a `sap.ui.model.type.Date` / `Float` / `Currency` to the binding, or write a formatter.

See [Binding → Data-Type Mapping](/cookbook/model/binding#data-type-mapping) for the type-mapping table and [Formatter](/cookbook/model/formatter) for the patterns.

## Write-Back Through a Typed Formatter

When `_bind( )` is wrapped in a `parts: [ … ], type: 'sap.ui.model.type.…'` binding, the type owns both directions — display and parse. If the value the user types does not match the formatter's expectations (locale, pattern, decimals, currency code), UI5 raises a parse exception and drops the write-back on the frontend. The ABAP attribute keeps its old value, and the next event arrives with stale data.

Where to look:
- **Browser console.** Warnings from `sap.ui.model.type` like `ParseException: Enter a valid value` or `Enter a valid date in the format …`.
- **Fix**: verify that `formatOptions` and `constraints` cover both the display and the parse direction, and that every entry in `parts` uses `path = abap_true` so the raw model path is injected — not the full `{…}` binding string.

See [Formatter](/cookbook/model/formatter).

## Malformed XML

`Z2UI5_CL_XML_VIEW` produces XML; UI5 parses it on the frontend. A typo in a control name, an unclosed tag, or an aggregation that contains an invalid child can break parsing entirely.

Where the error surfaces depends on what went wrong:
- **Pure XML syntax errors** (unclosed tag, bad escape) — the XML parser fails and UI5 logs a `Parse error` in the browser console. The page renders blank or up to the broken element.
- **Unknown UI5 controls / namespaces** — UI5 logs `failed to load 'sap.m.NotAControl'` (or similar) in the console; the surrounding view may render partially.
- **Wrong aggregation / wrong child type** — see the warning on the [View Definition](/cookbook/view/definition) page. UI5 may log an `aggregation … does not contain` warning or silently drop the child. Layouts can render in unexpected ways without any error.
- **ABAP side** — none of these surface as ABAP exceptions. `view_display( )` accepts any string. The response goes out, and only the browser notices.

When something looks wrong on screen, **always check the browser console first** before re-reading the ABAP code.

## Forgotten `view_display( )` After a Structural Change

abap2UI5 does not re-render automatically. After `check_on_init( )`, after a navigation return, or whenever the **structure** of the view has to change (different controls, a new dialog, a switch between screens), the handler must call `client->view_display( view->stringify( ) )`. Skip the call and the frontend keeps the previous view tree — the new controls never reach the browser.

Where to look:
- **Symptom**: blank page on first load (the `check_on_init` branch is missing `view_display`), or a button that "does nothing visible" even though state changed and the handler ran.
- **ABAP debugger**: confirm the `WHEN client->check_on_event( ... )` branch actually executes. The bug is almost always a missing `view_display( )` call, not a wrong event.

See [Life Cycle → The View Is Only Sent When You Call `view_display`](/cookbook/event_navigation/life_cycle#the-view-is-only-sent-when-you-call-view-display).

## Re-Rendering on Every Event

The opposite mistake: calling `view_display( )` from every event handler, even when only model data changed. The XML is rebuilt and re-sent on every roundtrip, which causes:
- visible flicker on each click,
- lost scroll position in tables,
- lost focus and cursor position in inputs,
- noticeably slower responses on large views.

Rebuild only when the view structure changes. For pure state mutations (an edit, a save, a row update, a popup close), mutate the public attribute and return — the framework re-serializes the model on every response and the view rebinds automatically.

See [Life Cycle → Lifecycle Pitfalls](/cookbook/event_navigation/life_cycle#lifecycle-pitfalls).

## Popup Not Destroyed

`client->popup_display( )` opens a dialog on top of the existing main view. The popup stays modal until the handler that finishes the workflow (Save, Cancel, OK) explicitly calls `client->popup_destroy( )`. Forget the destroy call and the dialog stays open over the next view, or the next click on the main page appears unresponsive because the click is captured by the modal layer.

Where to look:
- **Symptom**: a button on the main page "does nothing" right after a popup workflow, or a second open reopens the same popup on top of the first.
- **Pair every `popup_display( )` with an explicit `popup_destroy( )`** in **every** branch that ends the dialog — Save and Cancel both need it, not just the happy path.

See [Popup](/cookbook/popup_popover/popup) and the worked example in the walkthrough's [Popups step](/tutorials/walkthrough/step-7).

## Event-Name Casing Mismatch

`client->check_on_event( 'POST' )` matches only if the XML fires the event with the exact same string — case included. Typical mistakes: `press = client->_event( 'post' )` paired with `WHEN client->check_on_event( 'POST' )`, or stray whitespace from a string template. The `WHEN` branch is skipped, no handler runs, and the click appears to do nothing.

Where to look:
- **Browser network tab**: the request payload contains the event name as the browser sent it. Compare it character-for-character against your `WHEN` literal.
- **Prefer the `client->cs_event-*` constants** where the framework provides them (`open_new_tab`, `set_title`, `scroll_to`, …) over raw strings. For custom events, declare a constant in the class and reference both ends from the same source — typos then fail to compile instead of failing silently at runtime.

## State Lost Between Events

Between two events the app instance is serialized into a draft record on the **server** and read back on the next request; the browser only carries the draft id. **Attributes** of serializable types survive — at any visibility, `PROTECTED` and `PRIVATE` included. What does not survive is everything that is not an attribute or cannot be written: local variables, `DATA(...)` declarations inside an event handler, open database cursors, acquired locks, and `REF TO` references to non-serializable objects.

Where to look:
- **Symptom**: a value set in one event is empty on the next; a calculated value built up in `check_on_init` is gone by the time the user clicks; a singleton or "global" state appears to reset between roundtrips.
- **Fix**: move surviving state out of the method into an attribute with a concrete, serializable type — it does not have to be public for that; only `_bind( )` needs public. If serialization itself is the problem the roundtrip says so, with `APP_SERIALIZATION_ERROR`. For resources that genuinely need to live server-side across events (file handles, persistent locks, expensive caches), see [Statefulness](/cookbook/expert_more/statefulness).
- **The draft expires.** Four hours by default — an app left open longer starts fresh rather than restoring.

## Error Index

The sections above start from a behavior — nothing renders, data does not
update. This index starts from the other end: a literal message in front of
you, in the error view of a failed roundtrip or in the browser console. Every
message below is one the framework or UI5 actually produces; find yours, and
the entry says what caused it and where the fix is explained.

#### `The app 'ZCL_...' does not exist in the system.`

The framework could not instantiate the class named in `?app_start=` — a typo
in the URL, or the class exists but is not activated. The roundtrip answers
500 and shows this message. Fix the name or activate the class; the
[Quickstart's verify step](/get_started/quickstart#_5-verify) covers the other
first-launch failures around it.

#### `BINDING_ERROR - No class attribute for binding found - Please check if the bound values are public attributes of your class`

A `_bind( )` on an attribute that is not in the `PUBLIC SECTION`, or on a
local variable. Covered in full under
[Bound Attribute Not Public](#bound-attribute-not-public) above.

#### `BINDING_ERROR_TAB_CELL_LEVEL - Row index out of range`

A cell binding — `client->_bind( val = … tab = … tab_index = … )` — names a
row the table does not have: the index is off (it is 1-based, like every ABAP
index), or the table was refilled or shortened after the index was computed
and before the view was built. Rebuild the view from the current table state.

#### `Binding Error - component '...' not found in the bound row`

The same cell binding, but `val` is not a component of `tab`'s row type — the
classic case is passing a field of a *different* structure (a copy, a work
area of another type) as the cell value, or a renamed column that the view
code still names. Bind the field of the row type the table actually has.

#### `APP_SERIALIZATION_ERROR - the app state could not be serialized. Please check if all generic data references are public attributes of your class`

Between two events the app instance is serialized, and one attribute cannot
be: typically a `REF TO` a non-serializable object or a generic data
reference. The chained previous exception names the attribute that gave up.
Move non-serializable resources out of attributes, or see
[State Lost Between Events](#state-lost-between-events) above and
[Statefulness](/cookbook/expert_more/statefulness) for resources that must
live across events.

#### `Dispatch limit of 1000 app navigations in one request reached - check for an endless nav_app_call/nav_app_leave loop in main( )`

Two apps hand control to each other forever inside a single request — most
often a `nav_app_call( )` that runs unconditionally in `main( )` instead of
inside an event or `check_on_navigated( )` branch, so the called app's first
roundtrip immediately navigates again. Guard the navigation; see
[Navigation](/cookbook/event_navigation/navigation).

#### `failed to load 'sap/m/....js'` — browser console, view does not appear

UI5 resolved a tag in your view as a *control class* and requested a file
that does not exist. Two ways to get there: a control name that is not on the
UI5 release your system serves, or a generic child tag (`ele( 'footer' )`)
naming an aggregation the parent does not have **on that release** — UI5 then
tries to load it as a control. `sap.m.Dialog`'s `footer`, for example, is
public only in newer releases; on an older one, use `buttons`. The
[linter](/advanced/linter) decides both against the release you target,
without a system.

#### `Binding "/PATH" was not found in model` — browser console warning, control renders empty

Not an error — the control simply stays empty. Covered in full under
[Binding-Path Mismatch](#binding-path-mismatch) above.

#### `"" is of type string, expected <enum type> for property "..."` — browser console, app dies when a table empties

An enum-typed property (`type`, `state`, `valueState`, …) inside an
aggregation template is bound to a field that arrives as an empty string —
ABAP has no null, so an unfilled `TYPE string` serializes as `""`, and `""`
is a member of no UI5 enum. The first render passes; the failure comes when
the bound table is *emptied*, because UI5 then evaluates the template with no
row behind it. Keep initial values out of the model with `_bind( )`'s
`omit_initial_paths` (see the [Client API](/resources/api)), or give the
binding an explicit fallback to the enum's default value.

#### `EvalError: Evaluating a string as JavaScript violates the following Content Security Policy directive ...` — page loads, component does not start

A hardened CSP without `'unsafe-eval'` meets an old UI5 release: the `1.71`
ui5loader still evaluates module source as a string. Either bootstrap a
modern UI5 release, or keep `'unsafe-eval'` in the policy — see
[Security → Hardening](/configuration/security#hardening-dropping-unsafe-eval).

#### `403 ICFEUCONFORBIDDEN` — separately deployed frontend, every action fails

The deployed UI5 app posts to the HTTP service path written in its
`manifest.json`, and that service does not exist under this ID on your
system. The error does not say which URL it tried. Align the
`sap.app.dataSources.http.uri` entry with the service you created — see
[S/4 Public Cloud](/configuration/s4_public_cloud) for the paths each
frontend branch ships with.

#### `Literals across more than one line are not allowed` — abapGit pull, and the app class is empty afterwards

An import-time failure, not a runtime one: a source line longer than 255
characters. abapGit reports the error for that object and **carries on**, so
what stays behind is an empty class stub — the tree looks imported, the app
is gone. Pull again after the line is split (in your own code: break long
literals into `&&` chunks); check the abapGit log rather than the package
tree to see which objects really arrived.

#### An icon is simply missing — no message anywhere

An unknown `sap-icon://` name is not an error: the icon pool finds nothing
and the control renders without an icon, silently. Either the name does not
exist at all (icon names are effectively case-insensitive and matched
lower-cased, so a camelCase name matches nothing), or it entered the icon
font *after* the UI5 release your system serves. The
[linter](/advanced/linter) checks every icon name against the release you
target.

---

For EML-specific failure handling (`FAILED` / `REPORTED`, transactional behavior, `cx_abap_behv`, `cx_abap_lock_failure`, defensive `TRY/CATCH` patterns), see the [EML](/cookbook/eml_cds_sql/eml) page.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Basics V — The Developer Tools (Ctrl+F12) | [`Z2UI5_CL_SMP_APP_496`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_496.clas.abap) |

<!-- samples:end -->
