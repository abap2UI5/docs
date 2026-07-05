---
outline: [2, 4]
---
# Navigation

In abap2UI5, each app is a single ABAP class. You can pack all logic into one class, but keeping classes at a reasonable size is better practice. Splitting functionality into multiple interacting classes lets you build reusable apps and popups that work in different contexts.

## Cross App Navigation

### Backend
To call an ABAP class:
```abap
  METHOD z2ui5_if_app~main.

    DATA(lo_app) = NEW z2ui5_cl_new_app( ).
    client->nav_app_call( lo_app ).

ENDMETHOD.
```
The framework keeps a call stack. From the newly called class, return to the previous app with:
```abap
  METHOD z2ui5_if_app~main.

    client->nav_app_leave( ).

ENDMETHOD.
```
To read data from the previous app, cast it like this:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
        DATA(lo_called_app) = CAST z2ui5_cl_new_app( client->get_app_prev( ) ).
        client->message_box_display( `Input made in the previous app:` && lo_called_app->mv_input ).
        view_display( ).
    ENDIF.

ENDMETHOD.
```
::: warning Re-display your view on return
When the called app took over the screen with its own `view_display( )`, the browser still shows that view after `nav_app_leave( )` — the framework does not restore the previous view automatically. Call `view_display( )` again in the `check_on_navigated( )` branch. Your class attributes survived the roundtrip serialization, so no data re-read is needed — only the view must be rendered again. See [Life Cycle](/cookbook/event_navigation/life_cycle#returning-from-a-sub-app-hits-check_on_navigated-not-check_on_init).
:::
Called **with** an app instance, `nav_app_leave` works differently: it leaves the current app and starts the given one *without* pushing the current app onto the call stack — so there is nothing to return to. Use this to navigate forward while discarding the current app:
```abap
  METHOD z2ui5_if_app~main.

    DATA(lo_app) = NEW z2ui5_cl_new_app( ).
    client->nav_app_leave( lo_app ).

ENDMETHOD.
```
::: tip
Sounds familiar? The abap2UI5 framework echoes classic `call screen` and `leave to screen` behavior.
:::

For Launchpad-based cross app navigation, see the [Fiori Launchpad](/configuration/launchpad) page.

## Inner App Navigation

Use class attributes to track the current state and switch views as needed. This keeps navigation logic in a single ABAP class with no cross-app calls.
