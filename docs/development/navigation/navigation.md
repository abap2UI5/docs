---
outline: [2, 3]
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
    ENDIF.

ENDMETHOD.
```
To navigate to an app without pushing it onto the call stack:
```abap
  METHOD z2ui5_if_app~main.

    DATA(lo_app) = NEW z2ui5_cl_new_app( ).
    client->nav_app_leave( lo_app ).

ENDMETHOD.
```
::: tip
Sounds familiar? The abap2UI5 framework echoes classic `call screen` and `leave to screen` behavior.
:::

### Launchpad
We recommend backend communication only for view changes or popup calls. With a Launchpad, consider navigating via the Launchpad to use browser navigation and history:
```abap
client->_event_client(
    val   = client->cs_event-cross_app_nav_to_ext
    t_arg = VALUE #( (
        `{ semanticObject: "Z2UI5_CL_LP_SAMPLE_04",  action: "display" }`
    ) ) ).
```
For more on Launchpads and routing, see the [Fiori Launchpad](/configuration/launchpad) page.

## Inner App Navigation

Use class attributes to track the current state and switch views as needed. This keeps navigation logic in a single ABAP class with no cross-app calls.

## Next Steps

- **[App State](/development/navigation/app_state)** — preserve state across navigations and roundtrips.
- **[Share, Bookmark](/development/navigation/share)** — make app states deep-linkable.
