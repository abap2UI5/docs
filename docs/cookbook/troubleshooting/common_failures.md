---
outline: [2, 4]
---
# Common Failures

Not every problem raises an ABAP exception. Many failures surface only in the browser, fail silently, or look like framework bugs when they are actually pattern mistakes. The sections below cover the ten most common ones — what the symptom looks like and where to find the real cause.

## Binding-Path Mismatch

When a `_bind` path does not resolve against the JSON model on the frontend — typically because the public attribute was renamed, the data was never sent, or the path is mistyped — UI5 does **not** raise an ABAP exception. The control simply renders empty or with a default value.

Where to look:
- **Browser console.** UI5 logs a warning like `Binding "/path/to/field" was not found in model` from `sap.ui.model.json.JSONModel`. Open the browser DevTools console and filter by `sap.ui.model`.
- **Network tab.** Inspect the abap2UI5 response payload — the JSON model is included verbatim. If the field is absent or named differently than your binding path, you have your answer.
- **ABAP side.** Nothing. The backend never learns the binding failed. Asserting "no console warnings" is the only way to catch this in tests.

Two-way binding silently drops the write-back when the path is invalid — your ABAP attribute keeps its old value after the next event. Cross-check the attribute value in the debugger if data is mysteriously not updating.

## Bound Attribute Not Public

`_bind( )` and `_bind( )` resolve attributes via dynamic `ASSIGN` and only see the `PUBLIC SECTION`. Anything declared `PROTECTED` or `PRIVATE` is silently ignored — the binding path is generated, but no data is ever serialized for it. There is no compile-time or runtime error.

Where to look:
- **Symptom is identical to a binding-path mismatch.** The browser console shows the same `Binding "/path/..." was not found in model` warning, because the path was never populated on the wire.
- **Check the visibility of the attribute** in the class definition before re-reading the XML. Helper variables that never appear in a `_bind( )` call can stay private; anything you bind must move to `PUBLIC SECTION`.

See [Binding → Bound Attributes Must Be Public](/cookbook/model/binding#bound-attributes-must-be-public).

## Type Coercion Without an Explicit UI5 Type

ABAP and UI5 do not share a type system. A `d` field goes on the wire as `YYYYMMDD`, but `DatePicker` expects an ISO date. `abap_bool` arrives as `"X"`/`""`, but `CheckBox` expects `true`/`false`. Packed numbers arrive as strings without locale formatting. The control renders the raw value, parses it wrong, or treats it as empty.

Where to look:
- **Symptom**: a `DatePicker` shows `Invalid Date` or refuses input; a `CheckBox` is always unchecked even when the attribute is `abap_true`; numeric inputs lose decimals or render with the wrong separator.
- **Fix**: attach a `sap.ui.model.type.Date` / `Float` / `Currency` to the binding, or write a formatter.

See [Binding → Data-Type Mapping](/cookbook/model/binding#data-type-mapping) for the type-mapping table and [Formatter](/cookbook/model/formatter) for the patterns.

## Two-Way Binding Through a Typed Formatter

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

See [Popup](/cookbook/popup_popover/popup) and the worked example in [Full Example](/get_started/full_example).

## Event-Name Casing Mismatch

`client->check_on_event( 'POST' )` matches only if the XML fires the event with the exact same string — case included. Typical mistakes: `press = client->_event( 'post' )` paired with `WHEN client->check_on_event( 'POST' )`, or stray whitespace from a string template. The `WHEN` branch is skipped, no handler runs, and the click appears to do nothing.

Where to look:
- **Browser network tab**: the request payload contains the event name as the browser sent it. Compare it character-for-character against your `WHEN` literal.
- **Prefer the `client->cs_event-*` constants** where the framework provides them (`open_new_tab`, `set_title`, `scroll_to`, …) over raw strings. For custom events, declare a constant in the class and reference both ends from the same source — typos then fail to compile instead of failing silently at runtime.

## State Lost Between Events

Between two events the controller is serialized to the client and deserialized on the next request. Only `PUBLIC SECTION` attributes of **serializable** types survive — local variables, `DATA(...)` declarations inside an event handler, open database cursors, acquired locks, and `REF TO` references to non-serializable objects do not.

Where to look:
- **Symptom**: a value set in one event is empty on the next; a calculated value built up in `check_on_init` is gone by the time the user clicks; a singleton or "global" state appears to reset between roundtrips.
- **Fix**: move surviving state into the `PUBLIC SECTION` with concrete, serializable types. For resources that genuinely need to live server-side across events (file handles, persistent locks, expensive caches), see [Statefulness](/cookbook/expert_more/statefulness).

---

For EML-specific failure handling (`FAILED` / `REPORTED`, transactional behavior, `cx_abap_behv`, `cx_abap_lock_failure`, defensive `TRY/CATCH` patterns), see the [EML](/cookbook/eml_cds_sql/eml) page.
