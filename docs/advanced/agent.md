---
outline: [2, 4]
---
# AI-Assisted Development

This page is the entry point for any AI assistant (Claude, Copilot, Cursor, …) that should build abap2UI5 apps. It collects, in one place, every source of information the model needs and points to the canonical references in the three project repositories.

If you are pasting context into an AI tool, **start here**, then follow the links — the linked files (especially the `AGENTS.md` files in the source repositories) are kept up to date with every release and are the source of truth.

## The Three Repositories

abap2UI5 lives in three repositories. Each carries information an AI assistant should know:

| Repository | What's in it | Why an AI needs it |
|---|---|---|
| **[abap2UI5/abap2UI5](https://github.com/abap2UI5/abap2UI5)** | Core framework — interfaces, view builder, runtime | The public API (`z2ui5_if_app`, `z2ui5_if_client`, `z2ui5_cl_xml_view`) the app interacts with |
| **[abap2UI5/samples](https://github.com/abap2UI5/samples)** | Hundreds of working demo apps + app-development guide | Canonical patterns and a copy-pastable example for almost every UI5 control and feature |
| **[abap2UI5/docs](https://github.com/abap2UI5/docs)** | This documentation site | Conceptual background, lifecycle, cookbook recipes |

## Source-Repo Briefings (for Framework / Sample Contributors)

The two source repositories carry `AGENTS.md` briefings aimed at AI tools that work **inside those repos** — they are not needed for building apps:

- **[abap2UI5/AGENTS.md](https://github.com/abap2UI5/abap2UI5/blob/main/AGENTS.md)** — for AI assistants **modifying the framework itself**: layered architecture, coding rules, naming, abaplint setup, Clean-ABAP exceptions, public-API stability contract.
- **[samples/AGENTS.md](https://github.com/abap2UI5/samples/blob/main/AGENTS.md)** — for AI assistants **contributing sample apps**: folder scheme, formatting rules (blank lines, indentation), file/class conventions, abaplint setup.

If you only want to **build an abap2UI5 app**, this page is the single source — you do not need to read those files.

## Quick Briefing — The Mental Model

The minimum an AI must know to write working apps:

#### 1. One interface, one method

Every app implements `z2ui5_if_app` and has a single `main` method. The framework calls `main` on every HTTP roundtrip — initial load *and* every user interaction:

```abap
CLASS zcl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    " everything happens here
  ENDMETHOD.
ENDCLASS.
```

#### 2. Dispatch with `IF` / `ELSEIF`

`main` is a dispatcher. Three lifecycle checks decide what to do — always chain them with `IF` / `ELSEIF`, never separate `IF` blocks:

```abap
IF client->check_on_init( ).            " first call
  " load data + render view
ELSEIF client->check_on_navigated( ).   " returned from a called sub-app (nav_app_call), incl. built-in popup apps
  " re-display the view - the sub-app's view is still on screen; state survived serialization, no data re-read needed
ELSEIF client->check_on_event( `POST` ). " user fired event 'POST'
  " handle it
ENDIF.
```

→ [Life Cycle](/cookbook/event_navigation/life_cycle)

::: warning The #1 lifecycle bug: blank screen after navigation
Always call `view_display( )` in the `check_on_navigated( )` branch. When a called sub-app took over the screen and returns via `nav_app_leave( )`, the browser still shows the sub-app's view — the framework does not restore the previous view automatically. All class attributes survived the roundtrip serialization, so no data re-read is needed — just render the view again. Skipping this leaves the user on a blank or stale screen after navigating back.
:::

#### 3. State lives in **public** class attributes

Anything bound to the view must be `PUBLIC`. The framework serializes public attributes between roundtrips automatically — no session handling needed.

→ [Binding → Bound Attributes Must Be Public](/cookbook/model/binding#bound-attributes-must-be-public)

#### 4. `client` is the only framework API

Views, events, binding, navigation, messages — everything goes through the `client` reference (`z2ui5_if_client`):

| Category | Methods |
|---|---|
| Views | `view_display`, `view_destroy`, `view_model_update` |
| Popups / popovers | `popup_display`, `popup_destroy`, `popover_display`, `popover_destroy` |
| Binding | `_bind( var )` (two-way; `_bind_edit` is an obsolete alias of it) |
| Events | `_event( 'NAME' )`, `check_on_event( 'NAME' )`, `get_event_arg( )` |
| Navigation | `nav_app_call( app )`, `nav_app_leave( )`, `get_app_prev( )`, `set_nav_routing( )` |
| Messages | `message_box_display`, `message_toast_display` |
| Lifecycle | `check_on_init`, `check_on_event`, `check_on_navigated` |

Read the full interface at [`z2ui5_if_client.intf.abap`](https://github.com/abap2UI5/abap2UI5/blob/main/src/02/z2ui5_if_client.intf.abap).

#### 5. Views are XML — built with the fluent builder

Views are UI5 XML strings passed to `client->view_display( ... )`. Build them with **`z2ui5_cl_xml_view`** — the fluent builder with one ABAP method per UI5 control. Every sample app uses it; it is the only supported builder.

For any control or property the fluent API does not cover, drop down to its generic method `_generic( name = ... ns = ... t_prop = ... )`, which emits any XML element with any attributes — a 1:1 translation of the UI5 SDK. Both styles mix freely in the same view.

→ [View Definition](/cookbook/view/definition)

::: warning Do not use `z2ui5_cl_util_xml`
The former standalone XML builder `z2ui5_cl_util_xml` is **retired** (it lives in the framework's obsolete package and is scheduled for removal). No sample uses it anymore — do not generate new code with it. Use `z2ui5_cl_xml_view`, with `_generic( )` for anything it doesn't wrap.
:::

## Using the Builder: `z2ui5_cl_xml_view`

The fluent API follows a small set of conventions (details in [View Definition](/cookbook/view/definition#mapping-ui5-xml-abap-fluent-api)):

- Control methods are snake_case of the UI5 control class — `sap.m.MultiComboBox` → `->multi_combo_box( )`, `sap.m.Text` → `->text( )`.
- Properties are named parameters, lowercased without underscores (`enabled`, `placeholder`, `selectedkey`).
- Aggregations are methods named after the aggregation (`->items( )`, `->content( )`); calling one returns the builder so children chain inside it.
- `->stringify( )` serializes the view for `client->view_display( )`.

When a control, property or aggregation is missing from the fluent API, translate the UI5 SDK XML 1:1 with `_generic( )`:

```abap
view->_generic( name = `MultiComboBox` ns = `sap.m`
    t_prop = VALUE #( ( n = `selectedKeys` v = `{/keys}` )
                      ( n = `placeholder`  v = `Pick one` ) ) ).
```

::: warning Boolean attribute values
Always pass `'true'` or `'false'` as **string literals** — never `abap_true` / `abap_false`. `abap_false` (space) is silently dropped; `abap_true` (`'X'`) produces an invalid XML attribute value.
:::

Complete worked example: see [Quickstart](/get_started/quickstart), [Hello World](/get_started/hello_world), and [Full Example](/get_started/full_example).

## Canonical App Template

For anything beyond ~50 lines in `main`, extract `on_init` and `on_event` first, then add helpers (`view_display`, `data_read`, `data_update`) only when needed. Class names lowercase, no `FINAL`, definition order `TYPES` → `DATA` → `METHODS`. In the `check_on_navigated( )` branch call `view_display( )` directly — the state survived serialization, only the view is gone from the browser.

```abap
CLASS zcl_app_xxx DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    " bound data (public DATA attributes used by _bind)

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.        " first call: load data, render view
    METHODS on_event.       " user triggered an event
    METHODS view_display.   " build and render the view
    METHODS data_read.      " SELECT
    METHODS data_update.    " INSERT / UPDATE / DELETE

  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_app_xxx IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_navigated( ).
      view_display( ).
    ELSEIF client->check_on_event( ).
      on_event( ).
    ENDIF.

  ENDMETHOD.

  " on_init / on_event / view_display / data_read / data_update …

ENDCLASS.
```

The full template (with formatting and blank-line rules) lives in [samples/AGENTS.md](https://github.com/abap2UI5/samples/blob/main/AGENTS.md).

## Sample Apps — Hundreds of Working Examples

The fastest way for an AI to ground a generation in working code is to look up a similar app in the [samples repository](https://github.com/abap2UI5/samples/tree/main/src). Each `z2ui5_cl_demo_app_*` class is a self-contained, abapGit-installable app demonstrating a single feature or control.

Browse them online (no system needed): <https://abap2ui5.github.io/web-abap2ui5-samples/>

## Built-In Popups — Do Not Reinvent

Before building a custom dialog, check whether one of the built-in popup classes already covers the case:

`Z2UI5_CL_POP_TO_CONFIRM`, `Z2UI5_CL_POP_TO_INFORM`, `Z2UI5_CL_POP_TO_SELECT`, `Z2UI5_CL_POP_FILE_UL`, `Z2UI5_CL_POP_FILE_DL`, `Z2UI5_CL_POP_TABLE`, `Z2UI5_CL_POP_DATA`, `Z2UI5_CL_POP_TEXTEDIT`, `Z2UI5_CL_POP_PDF`, `Z2UI5_CL_POP_HTML`, `Z2UI5_CL_POP_IMAGE_EDITOR`, `Z2UI5_CL_POP_MESSAGES`, `Z2UI5_CL_POP_ERROR`, `Z2UI5_CL_POP_GET_RANGE`, `Z2UI5_CL_POP_GET_RANGE_M`, `Z2UI5_CL_POP_INPUT_VAL`, `Z2UI5_CL_POP_JS_LOADER`, `Z2UI5_CL_POP_DEMO_OUTPUT`

→ [Built-in popups](/cookbook/popup_popover/built_in)

## Deprecated UI5 Controls — Do Not Generate

The authoritative list is at <https://ui5.sap.com/#/api/deprecated>. The most common pitfalls for generated code:

**Whole libraries — never use any control from these:**

| Library | Deprecated since | Use instead |
|---|---|---|
| `sap.ui.commons.*` (Accordion, Button, CheckBox, ComboBox, DatePicker, Dialog, FileUploader, Label, Link, Menu, Panel, RadioButton, SearchField, Slider, TextArea, TextField, TextView, ToggleButton, Toolbar, Tree, Form, SimpleForm, AbsoluteLayout, BorderLayout, MatrixLayout, HorizontalLayout, VerticalLayout, … — entire library) | 1.38 | `sap.m` + `sap.ui.layout` |
| `sap.viz.ui5.*` legacy charts (Bar, Bubble, Bullet, Column, Combination, Donut, Heatmap, Line, Pie, Scatter, StackedColumn, Treemap, Waterfall, …) | 1.32 | `sap.viz.ui5.controls.VizFrame` |

**Individual deprecated controls:**

| Control | Deprecated since | Use instead |
|---|---|---|
| `sap.m.MultiEditField` | 1.120 | — |
| `sap.f.Avatar` | 1.73 | `sap.m.Avatar` |
| `sap.ui.core.XMLComposite` | 1.88 | Custom controls |
| `sap.ui.core.mvc.HTMLView` | 1.108 | `XMLView` |
| `sap.ui.core.mvc.JSONView` | 1.120 | `XMLView` |
| `sap.ui.core.mvc.JSView` | 1.90 | Typed views |
| `sap.ui.core.mvc.TemplateView` | 1.56 | `XMLView` |
| `sap.ui.core.tmpl.TemplateControl` | 1.56 | — |
| `sap.ui.table.ColumnHeader` | 1.120 | `sap.ui.table.Column` |
| `sap.ui.table.TableHelper` | 1.118 | — |
| `sap.f.routing.Router` / `Target` / `TargetHandler` / `Targets` | 1.56 | `sap.m.routing.*` (async) |
| `sap.tnt.IToolHeader` (interface) | 1.135 | Any control as `ToolPage` header |

**Deprecated enums/types to avoid:**

- `sap.m.ValueCSSColor`, `DateTimeInputType` (use `DatePicker` / `TimePicker`), `ListHeaderDesign`, `ListMode.SingleSelect` (1.143 → `SingleSelectLeft`), `FrameType.TwoThirds` / `Auto`, mis-spelled `PlacementType.*Prefered*` variants
- `sap.f.AvatarShape` / `AvatarSize` / `AvatarType` / `AvatarColor` / `AvatarImageFitType` / `IllustratedMessageType` / `IllustratedMessageSize` / `DynamicPageTitleArea` (use the `sap.m.*` equivalents)
- `sap.ui.layout.BlockBackgroundType.Mixed`, `form.GridElementCells`, `SimpleFormLayout.ResponsiveLayout`, `SimpleFormLayout.GridLayout`, `cssgrid.CSSGridGapShortHand`, `GridHelper`
- `sap.ui.table.NavigationMode`, `SortOrder` (use `sap.ui.core.SortOrder`), `VisibleRowCountMode` (use `rowMode` aggregation), `TreeAutoExpandMode`, `ResetAllMode`
- `sap.ui.core.MessageType` (use `module:sap/ui/core/message/MessageType`)
- `sap.ui.unified.ContentSwitcherAnimation` (1.147 — concept discarded)

**Other deprecated framework items:**

- Analysis Path Framework (APF) — deprecated 1.140
- `sap.m.PDFViewer.sourceValidationFailed()` — deprecated 1.141
- Declarative `data-sap-ui-type` attribute — deprecated 1.120 (use XML views)
- Belize, Blue Crystal, and Blue Crystal HCB themes — removed in 1.136 (use Horizon)

**Namespace caveat for `Avatar`:** when using `z2ui5_cl_xml_view->avatar( )`, leave `ns` empty so the element resolves to `sap.m.Avatar` via the View's default xmlns. **Never pass `ns = 'f'`** — that produces `<f:Avatar>`, which is the deprecated `sap.f.Avatar`. (`avatar_group` and `avatar_group_item` correctly use `ns = 'f'` because those controls still live in `sap.f`.)

## Documentation Map — What to Read When

When an AI needs deeper information than this page provides:

| Topic | Best reference |
|---|---|
| Architecture, request roundtrip | [Concept](/technical/concept), [How It All Works](/technical/how_it_all_works) |
| Lifecycle and `IF` / `ELSEIF` dispatcher pattern | [Cookbook → Life Cycle](/cookbook/event_navigation/life_cycle) |
| Building views, control choice | [Cookbook → View Definition](/cookbook/view/definition) |
| Data binding (`_bind`) | [Cookbook → Binding](/cookbook/model/binding) |
| Tables and trees | [Cookbook → Tables](/cookbook/model/tables), [Trees](/cookbook/model/trees) |
| Events, actions, exceptions | [Cookbook → Event, Navigation](/cookbook/event_navigation/life_cycle) |
| Hash routing, browser Back/Forward | [Cookbook → Routing](/cookbook/event_navigation/routing) |
| Popups and popovers | [Cookbook → Popup, Popover](/cookbook/popup_popover/popup) |
| Messages, toasts, i18n | [Cookbook → Translation, Messages](/cookbook/translation_messages/message) |
| Browser interaction (focus, scroll, URL) | [Cookbook → Browser Interaction](/cookbook/browser_interaction/title) |
| Device features (camera, geo, files) | [Cookbook → Device Capabilities](/cookbook/device_capabilities/info) |
| RAP / CDS / EML integration | [Cookbook → EML, CDS, SQL](/cookbook/eml_cds_sql/rap) |
| Snippets and recurring patterns | [Cookbook → Snippets](/cookbook/expert_more/snippets) |
| Installation / HTTP handler | [Quickstart](/get_started/quickstart) |

## A Ready-Made Prompt for AI Assistants

A prompt that gives an AI assistant enough context to produce a working app:

> You are building an abap2UI5 app. Read <https://abap2ui5.github.io/docs/advanced/agent.html> first — it is the single source of truth for app-building (template, client API, lifecycle, view builder, deprecated controls, documentation map). For working examples, browse <https://github.com/abap2UI5/samples/tree/main/src> (hundreds of apps, one feature per app). Use `z2ui5_cl_xml_view` as the view builder; for controls it does not wrap, look up the UI5 control at <https://ui5.sap.com/#/api> and translate the XML 1:1 with `_generic( )`. Do not use any control listed in this page's "Deprecated UI5 Controls" section. Always call `view_display( )` in the `check_on_navigated( )` branch — otherwise the screen is blank after returning from a called sub-app.

## Hard Rules (Cheat Sheet)

| Rule | Why |
|---|---|
| Implement `z2ui5_if_app` with a single `main` method | The only entry point the framework calls |
| Bound attributes must be `PUBLIC` | The framework binds via dynamic ASSIGN; `PROTECTED`/`PRIVATE` are silently ignored |
| Use `IF` / `ELSEIF` with `check_on_init` / `check_on_event( 'NAME' )` / `check_on_navigated` | Canonical dispatcher pattern |
| Always call `view_display( )` in the `check_on_navigated` branch | The browser still shows the called sub-app's view after `nav_app_leave( )` — without a re-display the screen stays blank |
| Use `z2ui5_cl_xml_view` for views (`_generic( )` for uncovered controls) | The only supported builder; `z2ui5_cl_util_xml` is retired |
| Pass XML boolean attributes as string literals `'true'` / `'false'` | `abap_true` / `abap_false` produce invalid or empty attributes |
| Use backtick string literals (`` ` ``), not single quotes | Project-wide convention enforced by abaplint |
| Never use a deprecated control | See the list on this page |
| Use built-in popups (`z2ui5_cl_pop_*`) before building custom ones | Tested, consistent, less code |
| Run `npx abaplint` on every change in the framework / samples repos | Primary quality gate; must report 0 issues |

That's the briefing. Hand the link to this page (or the two `AGENTS.md` files it points to) to any AI tool and it has everything it needs to build a working abap2UI5 app.
