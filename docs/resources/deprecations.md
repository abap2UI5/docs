---
outline: [2, 4]
---
# Deprecations

Things in abap2UI5 that have a successor. Every entry says what to write instead, with the old and the new code next to
each other.

::: tip Not the same as deprecated UI5 controls
This page is about **abap2UI5's own** API. For controls SAP has deprecated in
UI5 itself (`sap.ui.commons`, the legacy charts, the Belize themes …) see
[Deprecated Controls](/cookbook/view/deprecated_controls).
:::

## Start with the linter

Most of this page can be found — and part of it fixed — without reading
further. The [abap2UI5 linter](https://github.com/abap2UI5/linter) checks app
classes without an SAP system and rewrites the mechanical corrections in place:

```sh
npx @abap2ui5/linter src --fix          # correct what is mechanical, report the rest
npx @abap2ui5/linter src --fix-dry-run  # show what it would change, write nothing
```

| Finding | Covers | `--fix` |
|---|---|---|
| `obsolete-binder` | `_bind_edit( )` | rewrites to `_bind( )` — except a call carrying `custom_mapper_back` / `custom_filter_back`, which is reported only |
| `obsolete-model-update` | the five `*_model_update( )` methods | deletes the call |
| `obsolete-frontend-event` | `_event_client( )` | rewrites to `follow_up_action( )` |
| `unconverted-abap-boolean` | an ABAP boolean written into a view as `'X'` | moves it onto `a( b = … )` |
| `non-released-api` | anything outside the released `src/02` package | reported only |

What it cannot decide it leaves alone and reports, so a run is safe to repeat.

## Version status

The released version is **1.144.0**. Entries marked *next release* are already
on `main` but not in a release yet — they matter if you pull `main`, and they
tell you what is coming if you do not.

| What you have | What to write | Status |
|---|---|---|
| `view_model_update( )` and its four variants | delete the call | 1.143.0 |
| `_event_client( )` | `follow_up_action( )` | 1.143.0 |
| `_bind_edit( )` | `_bind( )` | 1.142.0 |
| `_bind( custom_mapper = … custom_filter = … )` | `omit_initial` / `omit_initial_paths` / `json`, or shape it in ABAP | 1.143.0 |
| `_bind( view = … )` | omit the parameter | 1.142.0 |
| `z2ui5_if_app~check_sticky` / `check_initialized` | `set_session_stateful( )` / `check_on_init( )` | **removed**, 1.143.0 |
| `set_nav_back( )` / `set_nav_routing( )` | `follow_up_action( )` | **removed**, 1.143.0 |
| `cs_event-nav_to_route` | `nav_app_call( )` | **removed**, 1.143.0 |
| `cs_event-history_back` | `nav_app_leave( )` or a raw expression | **removed**, 1.143.0 |
| `client->get( )-viewname` | delete the read | **removed**, 1.143.0 |
| `Formatter.round2DP` and four siblings | compute it in ABAP | **removed**, 1.143.0 |
| `z2ui5_cl_util_api*`, `z2ui5_cl_pop_bal` | `z2ui5_cl_util` / `z2ui5_cl_util_ext` | **removed**, 1.142.0 |
| `cs_event-wizard_set_next_step` | two `control_by_id` calls | 1.143.0 |
| `z2ui5_cl_xml_view` | `z2ui5_cl_ui5_view_builder` | 1.143.0 |
| built-in popups | the [popups add-on](https://github.com/abap2UI5-addons/popups) | 1.142.0 |
| `z2ui5.Util` | `z2ui5.Formatter` | 1.142.0 |
| `cs_config-title` | `cs_event-set_title` | 1.144.0 |
| `z2ui5_if_types=>…` | the same type on the object that uses it | 1.144.0 |
| `z2ui5_if_exit` | `z2ui5_if_ui5_exit` | 1.144.0 |

## Obsolete: still compiles

### The model-update methods do nothing

`view_model_update( )`, `nest_view_model_update( )`, `nest2_view_model_update( )`,
`popup_model_update( )` and `popover_model_update( )` are **empty methods**.

The model is pushed automatically now: the framework compares the model state
before `main( )` — taken after the incoming client deltas were applied — with
the state after `main( )` returned, and when they differ it sends the model to
every open view slot. A handler can no longer render stale by forgetting a call,
and an unchanged model sends nothing at all.

```abap
" old
METHOD z2ui5_if_app~main.
  CASE client->get( )-event.
    WHEN `BUTTON_POST`.
      ms_data-status = `posted`.
      client->view_model_update( ).      " <-- delete this
  ENDCASE.
ENDMETHOD.

" new
METHOD z2ui5_if_app~main.
  CASE client->get( )-event.
    WHEN `BUTTON_POST`.
      ms_data-status = `posted`.
  ENDCASE.
ENDMETHOD.
```

Delete the calls — they read as "the model is pushed here" where nothing
happens. `npx @abap2ui5/linter src --fix` removes them for you.

::: warning One case genuinely changed
What is gone with these methods is the ability to force an **unchanged** model
back onto the client. That had one legitimate use: resetting a control that
wrote a bound property on its own without sending it back. Rebuild the view with
`view_display( )` for that.
:::

### `_event_client( )` → `follow_up_action( )`

`follow_up_action( )` is the same call in the same position. It has a
`RETURNING` parameter, so a call whose result is consumed — the view-attribute
form — takes the same branch into `get_event_client( )` and emits the identical
roundtrip-free wire, byte for byte.

```abap
" old
)->a( n = `press` v = client->_event_client( val = client->cs_event-popup_close ) )

" new
)->a( n = `press` v = client->follow_up_action( val = client->cs_event-popup_close ) )
```

Arguments stay untouched, so this is a pure rename — which is why
`--fix` does it.

### `_bind_edit( )` → `_bind( )`

Earlier releases split binding into a display-only `_bind` and a writable
`_bind_edit`. That split is gone: there is only **one** binding left, and
`_bind_edit` is an alias of `_bind` with identical behaviour.

```abap
" old
value = client->_bind_edit( ms_data-name )

" new
value = client->_bind( ms_data-name )
```

`custom_mapper_back` / `custom_filter_back` are still accepted so the source
keeps compiling, but they are **no longer evaluated** — a call that passes one
is doing nothing with it. Drop the arguments together with the rename; that is
the one `_bind_edit` case `--fix` reports instead of rewriting, because removing
an argument is not a rename.

See [Data Binding](/cookbook/model/binding) for the full picture.

### `custom_mapper` / `custom_filter` on `_bind( )`

Both hand your app a reference into the bundled AJSON library — a mirrored copy
of an external project, not a contract abap2UI5 owns. An app implementing
`z2ui5_if_ajson_mapping` / `_filter` binds itself to whatever that mirror looks
like today.

Everything they were reached for now has a declarative counterpart on `_bind( )`:

```abap
" old - a filter that drops initial fields
client->_bind( val = ms_data custom_filter = NEW lcl_skip_initial( ) )

" new
client->_bind( val = ms_data omit_initial = abap_true )

" or scoped to the fields that need it - an abap_false that MUST reach the
" client is itself initial, so list the numeric/enum columns and leave booleans
client->_bind( val                = ms_data
               omit_initial_paths = VALUE #( ( `PRICE` ) ( `STATE` ) ) )
```

For a control property that must receive an **object** rather than a string,
`json = abap_true` splices a JSON node into the model. Anything else is better
shaped in ABAP before binding. Both parameters are still evaluated and keep
working.

### The `view` parameter of `_bind( )` / `_bind_edit( )`

Inert — it is not passed on internally and has no effect. It dates from the time
each view had its own model. Omit it.

```abap
" old
client->_bind( val = ms_data view = client->cs_view-popup )

" new
client->_bind( ms_data )
```

### `cs_event-wizard_set_next_step`

The event bundles the two calls a UI5 controller makes on a Wizard
(`discardProgress( oStep )` + `oStep.setNextStep( oNext )`) into one fixed pair.
Both methods are on the frontend's `CONTROL_METHODS` whitelist, so the same flow
is two ordinary `control_by_id` calls — which additionally reach `goToStep`, a
step the bundled event cannot express.

```abap
" old
client->follow_up_action( val   = client->cs_event-wizard_set_next_step
                          t_arg = VALUE #( ( `wizard` ) ( `step2` ) ) ).

" new
client->follow_up_action( val   = client->cs_event-control_by_id
                          t_arg = VALUE #( ( `wizard` ) ( `discardProgress` ) ( `step1` ) ) ).
client->follow_up_action( val   = client->cs_event-control_by_id
                          t_arg = VALUE #( ( `step1` ) ( `setNextStep` ) ( `step2` ) ) ).
```

The constant and its handler stay and keep working.

### The `nav_container_to` event family

`cs_event-nav_container_to` and its `nest_` / `nest2_` / `popup_` / `popover_`
variants still work — the backend maps them onto `cs_event-control_by_id` with
method `to`. New code can address the container directly:

```abap
" old
client->follow_up_action( val   = client->cs_event-nav_container_to
                          t_arg = VALUE #( ( `page2` ) ) ).

" new
client->follow_up_action( val   = client->cs_event-control_by_id
                          t_arg = VALUE #( ( `navcon` ) ( `to` ) ( `page2` ) ) ).
```

`cs_event-z2ui5` sits in the same obsolete group. It calls a function you
registered as a `z2ui5.*` global; passing the expression straight to
`follow_up_action( )` is the same call without the indirection:

```abap
" old
client->follow_up_action( val   = client->cs_event-z2ui5
                          t_arg = VALUE #( ( `myFunction` ) ) ).

" new
client->follow_up_action( `myFunction()` ).
```

It still works and is still dispatched. Note that both forms ship hand-written
JavaScript from the backend to the browser — read
[Raw JavaScript](/cookbook/expert_more/follow_up_action#raw-javascript) before
using either.

### `z2ui5_cl_xml_view` → `z2ui5_cl_ui5_view_builder`

`z2ui5_cl_ui5_view_builder` is the generic XML view builder that replaces the
typed wrapper methods of `z2ui5_cl_xml_view`: instead of one method per control
it builds any UI5 XML 1:1, so a control that has no wrapper is no longer a dead
end.

```abap
" old
DATA(view) = z2ui5_cl_xml_view=>factory( ).
view->page( title = `Hello` )->button( text = `Go` press = client->_event( `GO` ) ).

" new
DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
    )->ele( n = `View` ns = `mvc`
        )->a( n = `xmlns`     v = `sap.m`
        )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
        )->ele( `Page`
            )->a( n = `title` v = `Hello`
            )->tag( `Button`
                )->a( n = `text`  v = `Go`
                )->a( n = `press` v = client->_event( `GO` ) ).

client->view_display( view->stringify( ) ).
```

The chain is `factory` / `ele` / `tag` / `a` / `end` / `stringify`: `ele( )` adds
a child and descends into it, `tag( )` adds one and stays, `a( )` sets an
attribute on the element it follows, `end( )` ascends. One rule carries the
whole builder — `a( )` applies to the element the chain is **pointing at** — so
give an element its attributes before its first child.

::: tip Released in 1.143.0 — and nothing you have has to be rewritten
`z2ui5_cl_ui5_view_builder` ships in 1.143.0, and this documentation is written
against it throughout. `z2ui5_cl_xml_view` and `z2ui5_cl_xml_view_cc` are
**frozen, not removed**: they ship unchanged and keep working, so a view that
works today never has to be touched. Frozen means no new controls and no new
properties — write the current builder for anything new, and migrate an old
view when you are editing it anyway rather than for its own sake.

On 1.142.0 or older the typed builder is what you have; the chain above needs
1.143.0.
:::

::: tip It was called `z2ui5_cl_ai_xml` for a while
That class is gone. If you tried the old name, `z2ui5_cl_ui5_view_builder` is
the one to use.
:::

### Built-in popups → popups add-on

The built-in popup apps (`z2ui5_cl_pop_table`, `z2ui5_cl_pop_to_confirm`,
`z2ui5_cl_pop_messages`, `z2ui5_cl_pop_get_range`, …) moved into the frozen
package. Their successor is the separate
[popups add-on](https://github.com/abap2UI5-addons/popups), which is versioned on
its own instead of riding along with the framework.

```abap
" old - the frozen built-in
z2ui5_cl_pop_to_confirm=>factory( `Delete this entry?` )

" new - the add-on, installed separately with abapGit
z2ui5_cl_popup_to_confirm=>factory( `Delete this entry?` )
```

The name gains the two letters `up`: the add-on ships under `z2ui5_cl_popup_*`,
the frozen built-ins are `z2ui5_cl_pop_*`. The shipped classes are unchanged, so
existing calls keep compiling. See [Add-ons](/advanced/addons).

### Utility classes

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

### Invisible custom controls

Earlier versions of abap2UI5 needed an invisible helper control in the view for
every common browser interaction. Each one is a frontend event now, called from
ABAP with no control in the view at all:

| Control | Replacement |
|---|---|
| `Timer` | `cs_event-start_timer` — [Timer](/cookbook/browser_interaction/timer) |
| `Focus` | `cs_event-set_focus` — [Focus](/cookbook/browser_interaction/focus) |
| `Scrolling` | `cs_event-scroll_to` / `scroll_into_view` — [Scrolling](/cookbook/browser_interaction/scrolling) |
| `Title` | `cs_event-set_title` — [Title](/cookbook/browser_interaction/title) |
| `LPTitle` | `cs_event-set_title_launchpad` — [Title](/cookbook/browser_interaction/title) |
| `Favicon` | `cs_event-set_favicon` |
| `SoftKeyboard` | `cs_event-keyboard_set_mode` — [Soft Keyboard](/cookbook/browser_interaction/soft_keyboard) |
| `Info` | `client->get( )-s_device` / `-s_ui5` / `-s_focus` / `-s_scroll` — [Device Info](/cookbook/device_capabilities/info) |
| `History` | `client->set_push_state( )` — [URL Handling](/cookbook/browser_interaction/url_handling) |

The pattern is the same for all of them — drop the control from the view and
call the event after your event handler:

```abap
" old - an invisible control in the view carried the title
view->_generic( name = `Title` ns = `z2ui5` )->_cc_plain_xml( `Invoice 4711` ).

" new - no control, one call
client->follow_up_action( val   = client->cs_event-set_title
                          t_arg = VALUE #( ( `Invoice 4711` ) ) ).
```

The controls still ship and views that use them keep rendering. See
[Follow-up Action](/cookbook/expert_more/follow_up_action) for the full argument
list of each event.

### `cs_config-title` → `cs_event-set_title`

The page title used to be set in the user exit and the tab title while the app
runs with the `set_title` frontend event — two mechanisms for one string, which
could disagree about what the tab says. The one that stays is the one the app
can reach at any point in its life:

```abap
" old - in your z2ui5_if_exit implementation
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-title = `Invoice App`.

ENDMETHOD.

" new - in your app, whenever the title should change
client->follow_up_action( val   = client->cs_event-set_title
                          t_arg = VALUE #( ( `Invoice App` ) ) ).
```

The field stays on `cs_config` and an exit that assigns it still compiles — it
simply has no effect. The generated page carries a constant
`<title>abap2UI5</title>`, which is what the tab shows while UI5 boots, before
any app can speak. Inside a Fiori Launchpad shell the title is
`cs_event-set_title_launchpad`, unchanged. See
[Title](/cookbook/browser_interaction/title).

### `z2ui5_if_exit` → `z2ui5_if_ui5_exit`

The user-exit interface follows the framework's naming: everything that is the
engine rather than the contract carries the `ui5` segment. The two methods, the
three types and the behaviour are unchanged.

```abap
" old
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_exit.
ENDCLASS.

" new
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_ui5_exit.
ENDCLASS.
```

**Nothing has to change today.** Both interfaces ship, and abap2UI5 looks up
both when it searches for your exit class — a class implementing the old one is
found and called exactly as before. A class implementing *both* is called once,
through the new interface. The types on `z2ui5_if_exit` are references to the
ones on `z2ui5_if_ui5_exit`, not copies, so a config structure that gains a
field gains it under either name.

`z2ui5_if_exit` is deleted after a transition period, so move your exit over
when you next touch it — after the release that brings the new name, which the
examples on this site still wait for.

### `z2ui5_if_types` → the object that uses the type

`z2ui5_if_types` was a shared interface holding the types the API passes
around. Each of them now sits on the object whose signature is the reason it
exists, so the type you need is declared where you already are:

| What you have | What to write |
|---|---|
| `z2ui5_if_types=>ty_s_get` | `z2ui5_if_client=>ty_s_get` — the return type of `get( )` |
| `z2ui5_if_types=>ty_s_event_control` | `z2ui5_if_client=>ty_s_event_control` — the `s_ctrl` of `_event( )` |
| `z2ui5_if_types=>ty_s_name_value` / `ty_t_name_value` | `z2ui5_if_client=>ty_s_name_value` / `ty_t_name_value` |
| `z2ui5_if_types=>cs_device` | `z2ui5_if_client=>cs_device` |
| `z2ui5_if_types=>ty_s_http_context` / `ty_s_http_config` / `ty_s_http_config_post` | the same names on `z2ui5_if_exit`, whose two methods take them |
| `z2ui5_if_types=>ty_s_draft` | `z2ui5_cl_ui5_srv_draft=>ty_s_draft` |
| `z2ui5_if_types=>ty_s_config` | written out inside `z2ui5_if_client=>ty_s_get-s_config` |

```abap
" old
DATA ls_get TYPE z2ui5_if_types=>ty_s_get.

" new
DATA ls_get TYPE z2ui5_if_client=>ty_s_get.
```

Nothing was deleted and nothing was reshaped. `z2ui5_if_types` still ships,
unchanged, from the framework's frozen package — an app that names it compiles
and runs exactly as before, and every moved type is identical field for field,
so a variable declared the old way still fits the new signatures. There is no
deadline; change it when you next touch the class.

### `z2ui5.Util` → `z2ui5.Formatter`

`z2ui5.Util` (module `z2ui5/Util`) is a backward-compatible alias that
re-exports the date helpers from `z2ui5.Formatter` (module
`z2ui5/model/formatter`). It will not gain new helpers.

```abap
" old
|\{ path: `{ client->_bind( val = mv_date path = abap_true ) }`,
    formatter: 'z2ui5.Util.DateCreateObject' \}|

" new
|\{ path: `{ client->_bind( val = mv_date path = abap_true ) }`,
    formatter: 'Formatter.DateCreateObject' \}|
```

See [Formatter](/cookbook/model/formatter).
