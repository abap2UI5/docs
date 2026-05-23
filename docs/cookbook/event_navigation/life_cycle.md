---
outline: [2, 4]
---
# Life Cycle

abap2UI5 gives you great flexibility in how you structure apps. Most sample apps follow the pattern below. Use it as a starting point, and tweak it or build a wrapper around abap2UI5 for more specific behavior.

The idea: every request enters the `main` method, and you use `CASE` to dispatch between initialization, navigation returns, and user events. The recommended pattern uses `CASE abap_true` together with `client->check_on_init`, `client->check_on_event`, and `client->check_on_navigated` to dispatch to the matching handler method.

```abap
CLASS z2ui5_cl_demo_app_001 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mv_value  TYPE string.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS display_view.
    METHODS on_event.
    METHODS on_navigated.

  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_demo_app_001 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    CASE abap_true.
      WHEN client->check_on_init( ).
        on_init( ).
        display_view( ).
      WHEN client->check_on_event( ).
        on_event( ).
      WHEN client->check_on_navigated( ).
        on_navigated( ).
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
```

See the dedicated sections of this development guide for full details on views, events, data binding, and navigation.

## Lifecycle Pitfalls

A few details of the request lifecycle are easy to miss and produce bugs that look like framework issues but are actually pattern mistakes. These are not enforced by the compiler and not reported at runtime.

### Bound Attributes Must Be Public
Anything passed to `client->_bind( )` or `client->_bind_edit( )` must live in `PUBLIC SECTION` — the framework binds via dynamic ASSIGN and silently ignores `PROTECTED`/`PRIVATE` attributes. Helper variables that never appear in a `_bind( )` call can stay private. Details and rationale on [Binding → Bound Attributes Must Be Public](/cookbook/model/binding#bound-attributes-must-be-public).

### The View Is Only Sent When You Call `view_display`
abap2UI5 does not re-render the view automatically. After an event, if you do **not** call `client->view_display( ... )` again, the frontend keeps the previous view tree and only the model data is updated from the serialized state. This is the common case — most event handlers should mutate state and return, leaving the view alone.

Call `view_display( )` again only when the **structure** of the view needs to change: different controls, different bindings, a new dialog, navigation to a different screen. Rebuilding and re-sending the view on every event is wasteful and can cause visible flicker, lost scroll position, and lost focus.

### `check_on_event` Fires Once Per Roundtrip
Every HTTP request carries at most one event. `check_on_event( )` returns `abap_true` exactly once per call to `main`, for that single event. If the user clicks two buttons in quick succession, the framework dispatches them as two independent `main` invocations — they are never batched into one request.

Two consequences follow:
- **Do not assume event ordering inside one `main`.** You cannot look at "the previous event" from within an event handler; the previous event ran in a separate request and the work process has been released since.
- **State across events lives in public class attributes.** Between two events, abap2UI5 serializes the controller to the client and deserializes it on the next request. Anything stored in public attributes (and in serializable types) survives; local variables, open cursors, and acquired locks do not. For sessions that need surviving server-side resources, see [Statefulness](/cookbook/expert_more/statefulness).
