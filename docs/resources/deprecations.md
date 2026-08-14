---
outline: [2, 4]
---
# Deprecations

Things in abap2UI5 that still work but have a successor, plus the few that are
already gone. Nothing on this page breaks an existing app today unless it is
listed under [Removed](#removed) — the rest keeps compiling and running, it just
is no longer the way to write something new.

::: tip Not the same as deprecated UI5 controls
This page is about **abap2UI5's own** API. For controls SAP has deprecated in
UI5 itself (`sap.ui.commons`, the legacy charts, the Belize themes …) see
[Deprecated Controls](/cookbook/view/deprecated_controls).
:::

## Client API

### `_bind_edit( )` → `_bind( )`

Earlier releases split binding into a display-only `_bind` and a writable
`_bind_edit`. That split is gone: there is only **one** binding left — both
write to the same root model and behave identically, so `_bind_edit` is only an
alias.

```abap
" old
value = client->_bind_edit( ms_data-name )
" new
value = client->_bind( ms_data-name )
```

The one case that still needs `_bind_edit`: a mapping that differs per
direction. `_bind` has no `custom_mapper_back` / `custom_filter_back`
parameters.

See [Data Binding](/cookbook/model/binding) for the full picture.

### The `view` parameter of `_bind( )` / `_bind_edit( )`

Inert — it is not passed on internally and has no effect. It dates from the time
each view had its own model. Omit it.

### `nest_view_model_update( )` / `nest2_view_model_update( )` → `view_model_update( )`

A nested view owns no model. It is inserted into the main view's control tree
and inherits its model, so there is only ever one root model to refresh.

::: warning Worth changing, not just cosmetic
Up to and including 1.142.0 these two methods set a nest-only flag that the
frontend ignored whenever no nested view happened to be open — the call was a
**silent no-op** in that case. From the next release they delegate to
`view_model_update( )` and always refresh. Calling `view_model_update( )`
directly has always been correct.
:::

See [Nested Views](/cookbook/view/nested_views).

### Frontend events

| Constant | Status |
|---|---|
| `cs_event-image_editor_popup_close` | Belongs to the built-in image editor popup, which moved to the frozen package (see below) |
| `cs_event-nav_container_to` and the `nest_` / `nest2_` / `popup_` / `popover_` variants | Still work — the backend maps them onto `cs_event-control_by_id` with method `to`. New code can use `control_by_id` directly |

`cs_event-z2ui5` is **not** deprecated — it is the supported way to call your
own JavaScript functions, see [Custom JS](/cookbook/expert_more/custom_js).

## View builder

### `z2ui5_cl_xml_view` → `z2ui5_cl_ai_xml`

`z2ui5_cl_ai_xml` is the generic XML view builder that replaces the typed
wrapper methods of `z2ui5_cl_xml_view`: instead of one method per control it
builds any UI5 XML 1:1 from `open` / `leaf` / `a` / `shut` / `stringify`, so a
control that has no wrapper is no longer a dead end.

`z2ui5_cl_xml_view` and `z2ui5_cl_xml_view_cc` are frozen but ship unchanged —
existing apps keep working and there is no need to rewrite a working view.

::: tip The documentation still uses the old builder
Most cookbook pages are still written against `z2ui5_cl_xml_view`. They are
being migrated; until then the examples remain valid, because the frozen builder
still works.
:::

## Built-in popups → popups addon

The built-in popup apps (`z2ui5_cl_pop_table`, `z2ui5_cl_pop_to_confirm`,
`z2ui5_cl_pop_messages`, `z2ui5_cl_pop_get_range`, …) moved into the frozen
package. Their successor is the separate
[popups addon](https://github.com/abap2UI5-addons/popups), which is versioned on
its own instead of riding along with the framework.

The shipped classes are unchanged, so existing calls keep compiling. See
[Add-ons](/advanced/addons).

## Utility classes

`z2ui5_cl_util`, `z2ui5_cl_util_ext`, `z2ui5_cl_util_db`, `z2ui5_cl_util_http`,
`z2ui5_cl_util_log`, `z2ui5_cl_util_msg`, `z2ui5_cl_util_range`,
`z2ui5_cl_util_xml`, `z2ui5_cx_util_error` and the table `Z2UI5_T_91` are frozen.
Inside the framework they were replaced by an internal context class.

::: warning No drop-in successor for apps
There is no public replacement API for app code. The classes still ship and
still work; treat them as stable-but-closed rather than as something to migrate
away from today. Several pages of this documentation still use them
([Logon Language](/configuration/setup/logon_language),
[Lock](/cookbook/expert_more/lock),
[Spreadsheet](/cookbook/device_capabilities/spreadsheet)).
:::

## Invisible custom controls → frontend events

Eight invisible helper controls were replaced by frontend events that need no
control in the view at all:

| Control | Replacement |
|---|---|
| `Timer` | `cs_event-start_timer` — [Timer](/cookbook/browser_interaction/timer) |
| `Focus` | `cs_event-set_focus` — [Focus](/cookbook/browser_interaction/focus) |
| `Scrolling` | `cs_event-scroll_to` / `scroll_into_view` — [Scrolling](/cookbook/browser_interaction/scrolling) |
| `Title` | `cs_event-set_title` — [Title](/cookbook/browser_interaction/title) |
| `Favicon` | `cs_event-set_favicon` |
| `LPTitle` | `cs_event-set_title_launchpad` |
| `Info` | `client->get( )-s_device` / `-s_ui5` — [Device Info](/cookbook/device_capabilities/info) |
| `History` | `client->set_push_state( )` — [URL Handling](/cookbook/browser_interaction/url_handling) |

The controls still ship and views that use them keep rendering.

## JavaScript API

`z2ui5.Util` (module `z2ui5/Util`) is a backward-compatible alias that
re-exports the date helpers from `z2ui5.Formatter` (module
`z2ui5/model/formatter`). It will not gain new helpers — use `z2ui5.Formatter`
in new formatter code. See [Formatter](/cookbook/model/formatter).

## Removed

The only entries on this page that break a build.

### 1.142.0

| Object | Replacement |
|---|---|
| `z2ui5_cl_util_api`, `z2ui5_cl_util_api_c`, `z2ui5_cl_util_api_s` | The methods live on: `bal_*`, `tr_*`, `conv_get_itab_by_xlsx` / `conv_get_xlsx_by_itab` and `source_get_method` in `z2ui5_cl_util_ext`, the rest in `z2ui5_cl_util`. Only the class name is gone — replace `z2ui5_cl_util_api=>` with the class that now holds the method |
| `z2ui5_cl_pop_bal` | **No replacement.** The BAL message popup was dropped rather than moved into the frozen package like the other built-in popups. `z2ui5_cl_pop_messages` covers a similar case but has its own interface |

### Next release

| Object | Note |
|---|---|
| `z2ui5_if_types=>ty_s_get-viewname` | The framework never filled this component, so `client->get( )-viewname` always returned an empty string. Reading it no longer compiles — delete the read |
