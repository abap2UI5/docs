---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_495
  - z2ui5_cl_smp_app_004
---
# Life Cycle

Every request enters the `main` method. `CASE abap_true` dispatches between initialization, navigation returns, and user events using `` client->check_on_init( ) ``, `` client->check_on_event( `EVENT_NAME` ) ``, and `` client->check_on_navigated( ) ``. Each branch either does the work inline (tiny apps) or calls a named handler method (typical apps) — the **structure is always the same**.

```abap
CLASS z2ui5_cl_demo_app_001 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    " State — public so binding works (see Lifecycle Pitfalls below)
    DATA value TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.
    METHODS on_post.

  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_demo_app_001 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.
      WHEN client->check_on_init( ).
        value = `World`.        " one-time setup ...
        view_display( ).        " ... and the first screen
      WHEN client->check_on_navigated( ).
        view_display( ).        " back from a sub-app or a value help: hand a view back
      WHEN client->check_on_event( `POST` ).
        on_post( ).
    ENDCASE.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Page`
                )->a( n = `title` v = `Life Cycle`

                )->tag( `Input`
                    )->a( n = `value` v = client->_bind( value )
                )->tag( `Button`
                    )->a( n = `text`  v = `Post`
                    )->a( n = `press` v = client->_event( `POST` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD on_post.

    client->message_toast_display( |POST received: { value }| ).

  ENDMETHOD.

ENDCLASS.
```

Whether you dispatch with `CASE abap_true` (as above) or with an equivalent `IF` / `ELSEIF` chain (as the [Hello World](/get_started/hello_world) page and the first steps of the [Walkthrough](/tutorials/walkthrough/) do) is a matter of taste — the structure is what counts. Three things make this the recommended shape:

1. **Store `client` on `me->client`** so handler methods can use it without passing it around.
2. **Dispatch by event name** — `` check_on_event( `POST` ) `` rather than a generic `` CASE client->get( )-event `` with a second dispatch level. Each event gets its own `WHEN`. (With many events or extracted handler methods, the `CASE client->get( )-event` form is a fine alternative — the walkthrough's [last step](/tutorials/walkthrough/step-10) uses it.)
3. **One display method per view** — `view_display( )` above, called from every `WHEN` that should rebuild the screen. Two of them must: `check_on_init`, and `check_on_navigated`, which is the branch a return from a sub-app or a value help comes back through. Event handlers that only mutate state and reuse the existing view (a button press inside a popup, a toast) skip it — see [The View Is Only Sent When You Call `view_display`](#the-view-is-only-sent-when-you-call-view-display) below.
4. **`check_on_init` first, `check_on_navigated` after it.** Not alphabetical order and not taste: `check_on_init( )` means *this app instance has never run*, and every path to an instance's first `main( )` raises the navigated flag **as well**. The two are not exclusive, so in a `CASE abap_true` — where the first matching `WHEN` wins — putting the navigated branch first swallows the very first roundtrip and the one-time setup in the init branch never runs.

For a tiny app with one or two events, inline the view and the handler directly in the `WHEN` branches and skip the handler methods entirely. [Hello World](/get_started/hello_world) shows this variant; the walkthrough's [last step](/tutorials/walkthrough/step-10) shows the full version with multiple handler methods, a popup, and persistence. Both follow the same pattern — only the amount of code inside each branch differs.

## Lifecycle Pitfalls

A few details of the request lifecycle are easy to miss and produce bugs that look like framework issues but are actually pattern mistakes. None of them is enforced by the compiler, and all but the first go unreported at runtime.

### Bound Attributes Must Be Public
Anything passed to `client->_bind( )` must live in `PUBLIC SECTION` — the framework binds via dynamic ASSIGN and cannot see `PROTECTED`/`PRIVATE` attributes. This one *is* reported: the roundtrip fails with `BINDING_ERROR - No class attribute for binding found`. Helper variables that never appear in a `_bind( )` call can stay private. Details and rationale on [Binding → Bound Attributes Must Be Public](/cookbook/model/binding).

### The View Is Only Sent When You Call `view_display`
abap2UI5 does not re-render the view automatically. After an event, if you do **not** call `client->view_display( ... )` again, the frontend keeps the previous view tree and only the model data is updated from the serialized state. This is the common case — most event handlers should mutate state and return, leaving the view alone.

Call `view_display( )` again only when the **structure** of the view needs to change: different controls, different bindings, a new dialog, navigation to a different screen. Rebuilding and re-sending the view on every event is wasteful and can cause visible flicker, lost scroll position, and lost focus.

### Returning From a Sub-App Hits `check_on_navigated`, Not `check_on_init`
`check_on_init( )` is `abap_true` **exactly once** — on the very first call of an app instance. It does *not* fire again when control comes back to the app after a `nav_app_call( )` (a popup or a fullscreen sub-app) is closed with `nav_app_leave( )`. That return is signalled by `check_on_navigated( )`.

This trips up apps that build their view only under `check_on_init`:

```abap
" WRONG — screen is blank after returning from the sub-app
CASE abap_true.
  WHEN client->check_on_init( ).
    view_display( ).                " runs only the first time
  WHEN client->check_on_event( `OPEN_POPUP` ).
    client->nav_app_call( ... ).
ENDCASE.
```

When the sub-app is left, `main` runs again, but neither `check_on_init` nor any event matches, so `view_display( )` is never called and the user is left looking at a stale or empty screen. Nothing reports it: the response simply carries no view, and the browser keeps showing whatever was on it.

Display from the navigation-return branch as well. `check_on_navigated( )` covers strictly more than `check_on_init( )` does — the first display of the app raises it too, along with a `nav_app_leave( )`, a called sub-app or value help closing, and a bookmarked draft being restored — so an app with no one-time setup to do can react to it alone and drop the init branch entirely:

```abap
" CORRECT — the view is rebuilt on first display and on every return
CASE abap_true.
  WHEN client->check_on_navigated( ).
    view_display( ).
  WHEN client->check_on_event( `OPEN_POPUP` ).
    client->nav_app_call( ... ).
ENDCASE.
```

Reserve `check_on_init` for one-time setup that must *not* repeat on return — loading initial data, setting defaults — and keep it first in the `CASE`, as the sample at the top of this page does: it is the more specific of the two, and the more general one has to come after it. A fork whose init branch does nothing the navigated branch does not do is four lines saying what the navigated branch says alone.

As a rule of thumb: anything needed to **show the screen** belongs in a branch that also fires on navigation return.

### `check_on_event` Fires Once Per Roundtrip
Every HTTP request carries at most one event. `check_on_event( )` returns `abap_true` exactly once per call to `main`, for that single event. If the user clicks two buttons in quick succession, the framework dispatches them as two independent `main` invocations — they are never batched into one request.

Two consequences follow:
- **Do not assume event ordering inside one `main`.** You cannot look at "the previous event" from within an event handler; the previous event ran in a separate request and the work process has been released since.
- **State across events lives in class attributes.** Between two events abap2UI5 serializes the whole app instance and stores it **on the server** — a draft row in `z2ui5_t_01`, which expires after four hours by default. The browser carries the draft id, never the state.

  Every attribute survives, whatever its visibility: `PUBLIC` is what `_bind( )` needs in order to *reach* an attribute, not what keeps it alive. Unbound working state therefore belongs in `PROTECTED` — a bloated public section only makes every roundtrip serialize more. What does not survive is what the serializer cannot write: local variables, open cursors, acquired locks, and references to classes that are not serializable. For sessions that need surviving server-side resources, see [Statefulness](/cookbook/expert_more/statefulness).

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Basics III — Lifecycle: Init, Event, Navigated | [`Z2UI5_CL_SMP_APP_495`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_495.clas.abap) |
| Basics IV — Events, Views and Roundtrips | [`Z2UI5_CL_SMP_APP_004`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_004.clas.abap) |

<!-- samples:end -->
