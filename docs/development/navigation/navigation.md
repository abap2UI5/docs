---
outline: [2, 4]
---
# Navigation

In abap2UI5, each application is a single ABAP class. While you can put all logic into one class, keeping classes at a manageable size is better practice. Splitting functionality into multiple classes that interact with each other lets you build reusable applications and popups that work across different contexts.

### Cross App Navigation

#### Backend
To call an ABAP class, use the following code:
```abap
  METHOD z2ui5_if_app~main.

    DATA(lo_app) = NEW z2ui5_cl_new_app( ).
    client->nav_app_call( lo_app ).

ENDMETHOD.
```
The framework maintains a call stack. In the newly called class, you can return to the previous application using:
```abap
  METHOD z2ui5_if_app~main.

    client->nav_app_leave( ).

ENDMETHOD.
```
To access data from the previous application, use casting as follows:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
        DATA(lo_called_app) = CAST z2ui5_cl_new_app( client->get_app_prev( ) ).
        client->message_box_display( `Input made in the previous app:` && lo_called_app->mv_input ).
    ENDIF.

ENDMETHOD.
```
To navigate to an application without adding it to the call stack, use:
```abap
  METHOD z2ui5_if_app~main.

    DATA(lo_app) = NEW z2ui5_cl_new_app( ).
    client->nav_app_leave( lo_app ).

ENDMETHOD.
```
::: tip
Sound familiar? The abap2UI5 framework emulates the classic `call screen` and `leave to screen` behavior here.
:::

#### Launchpad
We recommend using backend communication exclusively for view changes or popup calls. With a Launchpad, consider navigating through the Launchpad to use browser navigation and history. Here's an example:
```abap
client->_event_client(
    val   = client->cs_event-cross_app_nav_to_ext
    t_arg = VALUE #( (
        `{ semanticObject: "Z2UI5_CL_LP_SAMPLE_04",  action: "display" }`
    ) ) ).
```
To learn more about Launchpads and routing, see the [Fiori Launchpad](/configuration/launchpad) documentation.

### Inner App Navigation

Use class attributes to track the current state and switch views accordingly. This keeps all navigation logic in a single ABAP class without cross-app calls.
