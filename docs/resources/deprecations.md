---
outline: [2, 4]
description: What in abap2UI5 has a successor - each deprecated call with what to write instead, old and new code side by side.
---
# Deprecations

Things in abap2UI5 that have a successor. Every entry says what to write
instead, with the old and the new code next to each other.

::: tip Not the same as deprecated UI5 controls
This page is about **abap2UI5's own** API. Controls SAP has deprecated in UI5
itself — `sap.ui.commons`, the legacy charts, the Belize themes — are not
abap2UI5's to deprecate and are not listed anywhere here: the framework passes
your XML through unchanged, so every one of them still renders. The
[linter](/advanced/linter) reports them against the release *your* system runs,
and [ui5.sap.com/#/api/deprecated](https://ui5.sap.com/#/api/deprecated) is the
always-current list.
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

The released version is **1.144.1**. Entries marked *next release* are already
on `main` but not in a release yet — they matter if you pull `main`, and they
tell you what is coming if you do not.

| What you have | What to write | Status |
|---|---|---|
| `view_model_update( )` and its four variants | delete the call | 1.143.0 |
| `_event_client( )` | `follow_up_action( )` | 1.143.0 |
| `_bind_edit( )` | `_bind( )` | 1.142.0 |
| `_bind( custom_mapper = … custom_filter = … )` | `omit_initial` / `omit_initial_paths` / `json`, or shape it in ABAP | 1.143.0 |
| `_bind( view = … )` | omit the parameter | **removed**, 1.144.1 |
| `cs_event-keyboard_set_mode` | the bound `inputMode` property of `z2ui5.cc.InputExt` — see [Soft Keyboard](../cookbook/browser_interaction/soft_keyboard) | **removed**, 1.144.1 |
| `cs_event-nav_container_to` and its `nest_` / `nest2_` / `popup_` / `popover_` variants | `cs_event-control_by_id` with method `to`, the slot as the `view` parameter | **removed**, 1.144.1 |
| the DDIC structure `Z2UI5_T_02` | name a type your own system has | **removed**, 1.144.1 |
| `cs_event-image_editor_popup_close` | `_event( arg = `$controller.slotValue('POPUP','myEditor','getImagePngDataURL')` )` plus the app's own `popup_destroy( )` | **removed**, 1.144.1 |
| `z2ui5_if_app~check_sticky` / `check_initialized` | `set_session_stateful( )` / `check_on_init( )` | **removed**, 1.143.0 |
| `set_nav_back( )` / `set_nav_routing( )` | `follow_up_action( )` | **removed**, 1.143.0 |
| `cs_event-nav_to_route` | `nav_app_call( )` | **removed**, 1.143.0 |
| `cs_event-history_back` | `nav_app_leave( )` or `cs_event-hash_back` | **removed**, 1.143.0 |
| `client->get( )-viewname` | delete the read | **removed**, 1.143.0 |
| `Formatter.round2DP` and four siblings | compute it in ABAP | **removed**, 1.143.0 |
| `z2ui5_cl_util_api*`, `z2ui5_cl_pop_bal` | `z2ui5_cl_util` / `z2ui5_cl_util_ext` | **removed**, 1.142.0 |
| `cs_event-wizard_set_next_step` | two `control_by_id` calls | **removed**, 1.144.1 |
| `z2ui5_cl_xml_view` | `z2ui5_cl_ui5_view_builder` | 1.143.0 |
| built-in popups | the [popups add-on](https://github.com/abap2UI5-addons/popups) | 1.142.0 |
| `z2ui5.Util`, `z2ui5.Formatter`, module `z2ui5/Util` | `core:require` of `z2ui5/model/formatter` | **removed**, *next release* |
| `cs_event-z2ui5` | a custom control in `z2ui5_ccc`, called by `cs_event-control_by_id` | **removed**, *next release* |
| `z2ui5_cl_pop_js_loader` | a custom control in `z2ui5_ccc` | **removed**, *next release* |
| custom JS reading `window.z2ui5` | nothing - the global is gone | **removed**, *next release* |
| `cs_config-title` | `cs_event-set_title` | 1.144.0 |
| `z2ui5_if_types=>…` | the same type on the object that uses it | 1.144.0 |
| `z2ui5_if_exit` | `z2ui5_if_ui5_exit` | 1.144.0 |
| `set_push_state( )`, `cs_event-set_push_state` | `hash_set( )`, `cs_event-hash_set` | **removed**, 1.144.1 |
| `set_app_state_active( )`, `cs_event-set_app_state_active` | `app_state_set_active( )`, `cs_event-app_state_set_active` | **removed**, 1.144.1 |
| `cs_event-set_nav_routing` | `cs_event-hash_routing` | **removed**, 1.144.1 |
| `cs_event-clipboard_app_state` | `app_state_get_href( )` + `cs_event-clipboard_copy` | **removed**, 1.144.1 |
| `_event( s_ctrl-check_allow_multi_req )` | `s_ctrl-check_queue_last` | **removed**, 1.144.1 |
| the UI5 options of `message_toast_display( )` / `message_box_display( )` | set them on the control, through `cs_event-control_global` | **removed**, 1.144.1 |

## Obsolete: still compiles

### The model-update methods do nothing

`view_model_update( )`, `nest_view_model_update( )`,
`nest2_view_model_update( )`, `popup_model_update( )` and
`popover_model_update( )` are **empty methods**.

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
`_bind_edit` is an alias of `_bind` with identical behavior.

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

**No AJSON type belongs in a bind call any more.** Both parameters hand your app
a reference into the bundled AJSON library — a mirrored copy of an external
project, not a contract abap2UI5 owns. An app implementing
`z2ui5_if_ajson_mapping` / `_filter` binds itself to whatever that mirror looks
like today, and a resync of the mirror is free to break it. The same goes for
`custom_mapper_back` / `custom_filter_back` on
`_bind_edit( )` (the section above), which are inert on top of that.

Everything they were reached for is declarative on `_bind( )` now, and each
replacement has a sample that proves it:

| What you used AJSON for | What to write instead | Sample |
|---|---|---|
| Drop initial fields so the UI5 default applies (`create_empty_filter`) | `omit_initial`, or `omit_initial_paths` for single columns | [`Z2UI5_CL_SMP_APP_507`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_507.clas.abap) |
| Get a model **node** instead of a quoted string — under keys no ABAP component could be named after | `json = abap_true` | [`Z2UI5_CL_SMP_APP_509`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_509.clas.abap) |
| Anything else | Shape the value in ABAP before you bind it | — |

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

```abap
" old - a mapper, so the model carries keys an ABAP structure cannot spell
client->_bind( val = ms_card custom_mapper = lo_manifest_mapper )

" new - the string already holds the JSON, so splice it in as a node
client->_bind( val = mv_card_manifest json = abap_true )
```

#### Nothing to migrate, in practice

Measured across [samples](https://github.com/abap2UI5/samples),
[samples-controls](https://github.com/abap2UI5/samples-controls) and
[samples-stack](https://github.com/abap2UI5/samples-stack): not one app class
passes either parameter or implements either interface, and none ever did in
their git history. If your own apps are the same — and most are — there is
nothing to change, only nothing new to write. The linter has no rule for this
one yet, so `grep -ri "custom_mapper\|custom_filter" src` is how you find out.

#### One gap, and one thing that stays

The single thing `_bind( )` deliberately cannot do is a mapping that differs
**per direction**. That only ever lived on `_bind_edit( )`, where the `_back`
halves are no longer evaluated, so it is not a working feature you would be
giving up.

And this is about the *interface*, not the library: AJSON stays. It is the model
engine behind every roundtrip, and `json = abap_true` is implemented with it.
What is going away is the leak of a mirrored library into the API your app
compiles against.

Both parameters are still evaluated and keep working — existing code does not
break.

### The `nav_container_to` event family — removed

`cs_event-nav_container_to` and its `nest_` / `nest2_` / `popup_` / `popover_`
variants are **gone**; an app that names one fails at compile time. They never
reached the frontend as events of their own — the backend rewrote each of them
into `cs_event-control_by_id` with method `to` — so the migration is to write
that call, which additionally reaches every *other* method of a NavContainer
instead of the single `to` the constants could express:

```abap
" old
client->follow_up_action( val   = client->cs_event-nav_container_to
                          t_arg = VALUE #( ( `navcon` ) ( `page2` ) ) ).

" new
client->follow_up_action( val   = client->cs_event-control_by_id
                          t_arg = VALUE #( ( `navcon` ) ( `to` ) ( `page2` ) ) ).
```

A container in another view slot took a variant of its own; now it is the
`view` parameter:

```abap
" old: cs_event-popup_nav_container_to
" new
client->follow_up_action( val   = client->cs_event-control_by_id
                          view  = client->cs_view-popup
                          t_arg = VALUE #( ( `navcon` ) ( `to` ) ( `page2` ) ) ).
```

One difference worth knowing when migrating the MAIN variant: `cs_view-main`
travels as the *empty* slot where the removed constant injected the literal
`MAIN`. An empty slot resolves the id across every open view, so it still finds
a container in the main view — it is wider, never narrower, and only an id that
exists in two open slots at once could tell the two apart.

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

The chain is `factory` / `ele` / `tag` / `a` / `end` / `stringify`: `ele( )`
adds a child and descends into it, `tag( )` adds one and stays, `a( )` sets an
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
existing calls keep compiling. See [Add-ons](/resources/addons).

### Utility classes

`z2ui5_cl_util`, `z2ui5_cl_util_ext`, `z2ui5_cl_util_db`, `z2ui5_cl_util_http`,
`z2ui5_cl_util_log`, `z2ui5_cl_util_msg`, `z2ui5_cl_util_range`,
`z2ui5_cl_util_xml`, `z2ui5_cx_util_error` and the table `Z2UI5_T_91` are
frozen. Inside the framework they were replaced by an internal context class.

::: warning No drop-in successor for apps
There is no public replacement API for app code. The classes still ship and
still work; treat them as stable-but-closed rather than as something to migrate
away from today. No page of this documentation uses them any more: what the
examples reached them for is SAP standard — `cl_web_http_utility` for base64
(`cl_http_utility=>if_http_utility~encode_x_base64( )` on older releases), and
a failing library raises its own exception instead of being wrapped. For the
JSON they were also reached for, see the section below.
:::

### JSON is built and read by hand

**There is deliberately no released JSON parser or writer, and there is no
substitute to reach for.** `/ui2/cl_json` is not released for ABAP Cloud,
`xco_cp_json` is missing on 7.02, and the bundled [ajson](/technical/tools/ajson)
is the framework's model engine — a mirrored copy of an external project, which
the abap2UI5 linter reports as `non-released-api` when an app names it, and it
is right to.

There was a released reader, `z2ui5_cl_ui5_json`, for two weeks in September
2026. It was removed again before it had shipped in any release, so no
installation ever had it and nothing has to be migrated away from it.

**Outbound — a control property that must receive an object.** A
`sap.ui.integration` Card manifest is the case this exists for: its keys
(`sap.app`, `sap.card`) are not valid ABAP field names, so no typed ABAP value
can be that object, and UI5 reads a *string* manifest as a manifest URL. Keep
the JSON in a `string` attribute, compose it in ABAP, and bind it as a node:

```abap
client->_bind( val = mv_card_manifest json = abap_true )
```

A string that does not parse raises rather than shipping broken JSON to the
frontend. This direction is outbound only — the attribute is not read back, so
ABAP stays the single author of it. Where the payload has a URL instead, bind
that; it needs no flag.

**Inbound — an event argument that arrives as JSON.** Write the few lines that
read the field you need. `Z2UI5_CL_SMP_APP_327`
([`json_get_value`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_327.clas.abap),
one field of a flat object) and `Z2UI5_CL_SMP_APP_197`
([`json_get_values`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_197.clas.abap),
one property across an array of objects) are the pattern to copy: find
`"<name>":"` and take what stands up to the next quote.

Two limits worth knowing before you copy it:

- **It is a reader for FLAT payloads.** A control-valued event parameter is
  marshaled as an object of the control's `ID` plus its public properties, and
  a property whose value is itself an object or an array travels as one — the
  frontend passes such a value through rather than flattening it. A `find` walk
  answers the flat case; a nested one is a sign to bind the value into the model
  instead of parsing it out of an event argument, which is what a two-way bound
  attribute does for you with no parsing at all.
- **A payload composed from user input needs escaping on both ends.** The
  samples above read what the *framework* wrote. If your own app also writes the
  JSON — publishing into an AMC channel, say — then a quote the user typed
  reaches the reader as `\"`, and a `substring_before` on the next quote ends
  the value early.
  `Z2UI5_CL_SMPS_APP_489`
  ([samples-stack](https://github.com/abap2UI5/samples-stack/blob/main/src/07/z2ui5_cl_smps_app_489.clas.abap))
  is that case written out: a writer that escapes and a reader that walks the
  value resolving escapes.

An app parsing genuinely arbitrary, nested JSON is doing something this
framework does not hand it a tool for.

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
| `Info` | `client->get( )-s_device` / `-s_ui5` / `-s_focus` / `-s_scroll` — [Device Info](/cookbook/device_capabilities/info) |
| `History` | `client->hash_set( )` — [URL Handling](/cookbook/browser_interaction/url_handling) |

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
[Frontend](/cookbook/event_navigation/frontend) for the full argument
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
three types and the behavior are unchanged.

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
when you next touch it. The new name shipped with 1.144.0, and every exit
example on this site is written against it.

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
| `z2ui5_if_types=>ty_s_http_context` / `ty_s_http_config` / `ty_s_http_config_post` | the same names on `z2ui5_if_ui5_exit`, whose two methods take them |
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

## Removed: does not compile any more

Every name below is **gone from the framework**, not merely marked. A call
that still writes one fails at activation, so these are the entries to work
through before you pull a release that carries them — the table above says
which one that is.

### The `view` parameter of `_bind( )` / `_bind_edit( )`

**Removed.** It dates from the time each view slot had a model of its own; today
one model serves every open slot, so there was nothing left for it to select —
the parameter was inert, never passed on internally, for as long as it carried
the obsolete mark. Deleting it is the whole migration, and a call that still
names it does not compile.

```abap
" old
client->_bind( val = ms_data view = client->cs_view-popup )

" new
client->_bind( ms_data )
```

The binding string that comes back is the one that came back before. `cs_view`
itself stays — it is the view slot of `follow_up_action( )`, where it does
select one.

### `cs_event-wizard_set_next_step`

**Removed.** The event bundled the two calls a UI5 controller makes on a Wizard
(`discardProgress( oStep )` + `oStep.setNextStep( oNext )`) into one fixed pair.
Both methods are on the frontend's `CONTROL_METHODS` whitelist, so the same flow
is two ordinary `control_by_id` calls — which additionally reach `goToStep`, a
step the bundled event could not express.

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

The constant is gone from `cs_event` and the `WIZARD_SET_NEXT_STEP` handler is
gone from the frontend, so a call that still names either does not compile and
a raw string does nothing.

### The URL API is `hash_*` and `app_state_*` now

<Badge type="tip" text="1.144.1" />

One naming rule for everything that touches the URL, taken from UI5's own:
`nav_*` keeps meaning real navigation between apps, `hash_*` is the URL
fragment (named after `sap/ui/core/routing/HashChanger`), and `app_state_*` is
the state the URL carries.

| What you have | What to write | Why the name changed |
|---|---|---|
| `client->set_push_state( )` | `client->hash_set( )` | `HashChanger#setHash`. "Push state" described the old `history.pushState` implementation, which the HashChanger-backed write replaced |
| `client->set_app_state_active( )` | `client->app_state_set_active( )` | the family prefix, so it sorts next to `app_state_get_href( )` |
| `cs_event-set_nav_routing` | `cs_event-hash_routing` | it is the *hash* that is being routed; `nav_*` is app-to-app navigation |
| `cs_event-clipboard_app_state` | `app_state_get_href( )` + `cs_event-clipboard_copy` | the backend composes the link now, so the app can show or mail it, not only copy it |

The first two also had an *event* spelling — `cs_event-set_push_state` and
`cs_event-set_app_state_active`, alias constants for `cs_event-hash_set` and
`cs_event-app_state_set_active`. They are removed with the methods, so the
whole family is `hash_*` / `app_state_*` and nothing else.

**Removed.** Everything but the last shared its wire value with the surviving
name — the old spelling and the new one reached the same branch — so there was
no behavior to migrate, only a name, and a call that still writes an old one
does not compile:

```abap
" before
client->set_push_state( `&my-app-state=detail` ).
client->follow_up_action( client->cs_event-set_nav_routing ).

" after
client->hash_set( `&my-app-state=detail` ).
client->follow_up_action( client->cs_event-hash_routing ).
```

`cs_event-clipboard_app_state` is the one that was not a pure rename: it
composed the link in the browser and could only put it on the clipboard, and
its frontend handler is gone with the constant. The replacement hands the
string to the backend, which is what lets an app show it in an `Input`, mail it
or render it as a QR code — and the composed link keeps a Fiori Launchpad's
shell hash, so a recipient lands in the app instead of on the launchpad home
page:

```abap
" before - fire and forget, the link never existed in ABAP
client->follow_up_action( client->cs_event-clipboard_app_state ).

" after - the app owns the string
share_link = client->app_state_get_href( ).
client->follow_up_action( val   = client->cs_event-clipboard_copy
                          t_arg = VALUE #( ( share_link ) ) ).
```

What is genuinely new rather than renamed — `hash_replace( )`,
`cs_event-hash_back` and `cs_event-hash_attach_changed`, which together give an
app the URL semantics of a UI5 router — is on
[Hash](/cookbook/event_navigation/navigation/hash).

### `check_allow_multi_req` → `check_queue_last`

`s_ctrl-check_allow_multi_req` sent the event while another round-trip was
still running. Every firing went out at once and the responses could land in
any order — but only the newest response may commit its result, so each earlier
round-trip was work the backend did to have its answer thrown away.

The case it was reached for is the wire that fires per keystroke — `liveChange`,
`liveSearch`, `sliderChange` — and `check_queue_last` serves that one properly:
while a round-trip is in flight the **last** event fired on the wire is kept and
dispatched once the response has landed. One round-trip at a time, order
preserved, and the backend ends on the control's current value.

```abap
" old - one round-trip per keystroke, responses in any order
)->a( n = `liveChange` v = client->_event(
    val    = `SEARCH`
    s_ctrl = VALUE #( check_allow_multi_req = abap_true ) )

" new - one round-trip at a time, the last keystroke survives
)->a( n = `liveChange` v = client->_event(
    val    = `SEARCH`
    s_ctrl = VALUE #( check_queue_last = abap_true ) )
```

The component is gone from `ty_s_event_control`, so an app that names it does
not activate — the rename above is the whole migration. Without either flag the
busy guard drops every event fired during a flight, which is right for a click
and wrong for a keystroke.

### The UI5 options of `message_toast_display( )` / `message_box_display( )`

**Removed.** Both methods had grown to fourteen parameters each, and most of
each list was a plain `sap.m` option abap2UI5 did nothing with but pass on. What
the ABAP method carries is what an ABAP app decides — the data in whatever shape
it has, the kind of box, the buttons, the backend event its closing raises.
Where a toast docks and how wide a box is are not ABAP decisions, and they are
set on the control now.

| Method | Parameters gone |
|---|---|
| `message_toast_display( )` | `width`, `my`, `at`, `of`, `offset`, `collision`, `autoclose`, `animationtimingfunction`, `animationduration`, `closeonbrowsernavigation`, `class` |
| `message_box_display( )` | `textdirection`, `icon`, `closeonnavigation`, `dependenton`, `contentwidth` |

This is the one entry on this page that shortens a **signature** rather than
retiring a name, so a call that still passes one of them fails on the parameter,
not on the method. What stays is `text`, `type`, `title`, `styleclass`,
`actions`, `emphasizedaction`, `initialfocus`, `details` and `onclose` on the
box, and `text`, `duration` and `onclose` on the toast.

The replacement is the whitelisted global call, whose last argument is the
option object of `sap.m.MessageToast.show( )` / `sap.m.MessageBox.<type>( )`
1:1 — an argument that starts with a brace travels as real JSON:

```abap
" old
client->message_box_display( text         = `Not saved.`
                             type         = `error`
                             contentwidth = `30rem`
                             icon         = `WARNING` ).

" new - the display method IS the box type
client->follow_up_action(
    val   = client->cs_event-control_global
    t_arg = VALUE #( ( `MESSAGE_BOX` )
                     ( `error` )
                     ( `Not saved.` )
                     ( `{"contentWidth":"30rem","icon":"WARNING"}` ) ) ).
```

Both paths end in the same frontend code, so the options behave identically:
`onClose` stays a backend event name on either, the details are still expanded,
`dependentOn` is still resolved to a control. The raw path can do one thing
more — wired into a view it needs no round-trip at all. See
[Message](/cookbook/translation_messages/message).

### The `z2ui5` frontend global

**Removed.** The frontend kept its state on a global object, `window.z2ui5`, and
apps could reach into it from custom JavaScript. That object is gone: the
frontend keeps its state to itself, and nothing abap2UI5 ships puts anything on
`window` any more. Everything that existed only to reach the global went with
it.

`cs_event-z2ui5` called a function you had registered as a `z2ui5.*` member.
Nothing runs hand-written JavaScript from the backend any more - the raw
expression form of `follow_up_action( )` went with the global. What the
function did belongs in a
[custom control](/advanced/extensibility/custom_control) in the customer
frontend BSP (`z2ui5_ccc`), reached like any other control:

```abap
" old
client->follow_up_action( val   = client->cs_event-z2ui5
                          t_arg = VALUE #( ( `myFunction` ) ) ).

" new - the function is a method of a custom control in the view
client->follow_up_action( val   = client->cs_event-control_by_id
                          t_arg = VALUE #( ( `myControl` ) ( `myFunction` ) ) ).
```

The UI5 globals need no control of their own: `cs_event-control_global`
reaches `MessageToast`, `MessageBox`, `BusyIndicator` and the rest - see
[Frontend](/cookbook/event_navigation/frontend#calling-control-methods-on-the-frontend).

`z2ui5_cl_pop_js_loader`, the built-in popup that loaded such a function onto
the global, is removed with it; the popup had nothing left to write into.

The `z2ui5.Util` and `z2ui5.Formatter` globals, and the `z2ui5/Util` module,
are gone too. The date helpers live in `z2ui5/model/formatter`, loaded on the
view root with `core:require` (UI5 1.74 and later):

```abap
" old
|\{ path: `{ client->_bind( val = mv_date path = abap_true ) }`,
    formatter: 'z2ui5.Util.DateCreateObject' \}|

" new - view->a( n = `core:require` v = `{Formatter: 'z2ui5/model/formatter'}` )
|\{ path: `{ client->_bind( val = mv_date path = abap_true ) }`,
    formatter: 'Formatter.DateCreateObject' \}|
```

See [Formatter](/cookbook/model/formatter).
