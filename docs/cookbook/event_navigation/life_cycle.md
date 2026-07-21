---
outline: [2, 4]
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

    METHODS render_main.
    METHODS on_post.

ENDCLASS.

CLASS z2ui5_cl_demo_app_001 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.
      WHEN client->check_on_init( ).
        render_main( ).
      WHEN client->check_on_event( `POST` ).
        on_post( ).
      WHEN client->check_on_navigated( ).
        " optional: refresh state when returning from another app
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
```

Whether you dispatch with `CASE abap_true` (as above) or with an equivalent `IF` / `ELSEIF` chain (as the [Hello World](/get_started/hello_world) and [Full Example](/get_started/full_example) tutorials do) is a matter of taste — the structure is what counts. Three things make this the recommended shape:

1. **Store `client` on `me->client`** so handler methods can use it without passing it around.
2. **Dispatch by event name** — `` check_on_event( `POST` ) `` rather than a generic `` CASE client->get( )-event `` with a second dispatch level. Each event gets its own `WHEN`. (With many events or extracted handler methods, the `CASE client->get( )-event` form is a fine alternative — the [Full Example](/get_started/full_example) uses it.)
3. **One render method per view** — call it from any `WHEN` that should rebuild the screen (`check_on_init`, after a search, after a navigation return). Event handlers that only mutate state and reuse the existing view (a button press inside a popup, a toast) skip it — see [The View Is Only Sent When You Call `view_display`](#the-view-is-only-sent-when-you-call-view-display) below.

For a tiny app with one or two events, inline the view and the handler directly in the `WHEN` branches and skip the handler methods entirely. [Hello World](/get_started/hello_world) shows this variant; [Full Example](/get_started/full_example) shows the full version with multiple handler methods, a popup, and persistence. Both follow the same pattern — only the amount of code inside each branch differs.

## Lifecycle Pitfalls

A few details of the request lifecycle are easy to miss and produce bugs that look like framework issues but are actually pattern mistakes. These are not enforced by the compiler and not reported at runtime.

### Bound Attributes Must Be Public
Anything passed to `client->_bind( )` or `client->_bind( )` must live in `PUBLIC SECTION` — the framework binds via dynamic ASSIGN and silently ignores `PROTECTED`/`PRIVATE` attributes. Helper variables that never appear in a `_bind( )` call can stay private. Details and rationale on [Binding → Bound Attributes Must Be Public](/cookbook/model/binding#bound-attributes-must-be-public).

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
    render_main( ).                 " runs only the first time
  WHEN client->check_on_event( `OPEN_POPUP` ).
    client->nav_app_call( ... ).
ENDCASE.
```

When the sub-app is left, `main` runs again, but neither `check_on_init` nor any event matches, so `view_display( )` is never called and the user is left looking at a stale or empty screen. Render from the navigation-return branch as well — `check_on_navigated( )` fires both on the first display *and* on every return, so reacting to it alone is usually enough:

```abap
" CORRECT — the view is rebuilt on first display and on every return
CASE abap_true.
  WHEN client->check_on_navigated( ).
    render_main( ).
  WHEN client->check_on_event( `OPEN_POPUP` ).
    client->nav_app_call( ... ).
ENDCASE.
```

Reserve `check_on_init` for one-time setup that must *not* repeat on return (loading initial data, setting defaults). As a rule of thumb: anything needed to **show the screen** belongs in a branch that also fires on navigation return.

### `check_on_event` Fires Once Per Roundtrip
Every HTTP request carries at most one event. `check_on_event( )` returns `abap_true` exactly once per call to `main`, for that single event. If the user clicks two buttons in quick succession, the framework dispatches them as two independent `main` invocations — they are never batched into one request.

Two consequences follow:
- **Do not assume event ordering inside one `main`.** You cannot look at "the previous event" from within an event handler; the previous event ran in a separate request and the work process has been released since.
- **State across events lives in public class attributes.** Between two events, abap2UI5 serializes the controller to the client and deserializes it on the next request. Anything stored in public attributes (and in serializable types) survives; local variables, open cursors, and acquired locks do not. For sessions that need surviving server-side resources, see [Statefulness](/cookbook/expert_more/statefulness).
