---
outline: [2, 4]
description: Authorization for abap2UI5 apps at the service level or in the app class, with the authorization objects and checks you already use in ABAP.
---
# Authorization

abap2UI5 gives you flexibility in managing authorization. It has no built-in authorization mechanism, so you can build your own solution at either the service or the app level.

## Service-Level
The HTTP handler decides **who reaches abap2UI5 at all** - not which app runs. Logon procedure, SSO and the authorization field of the node in `SICF` (checked against `S_ICF`) work as on any other ICF service, and need no code.

### The ICF node's authorization field
Every ICF node has a field *SAP Authorization* on its *Service Data* tab in `SICF`. A node with a value there is reachable only by a user whose roles carry the authorization object `S_ICF` with `ICF_FIELD = SERVICE` and `ICF_VALUE` equal to that value; a node without a value is reachable by every user who can log on. So, without any code:

1. In `SICF`, open the node of your abap2UI5 service (`/sap/bc/z2ui5`, or one of your own), tab *Service Data*, and enter a value in *SAP Authorization*, for example `Z2UI5_SALES`.
2. In `PFCG`, add `S_ICF` to the role of the users who may use the apps behind that node: `ICF_FIELD` = `SERVICE`, `ICF_VALUE` = `Z2UI5_SALES`.
3. A user without that value gets the standard *not authorized* page of the ICF before any ABAP of yours runs.

The check is per node, so the recommended pattern is **one node per area and an app-level check inside the apps**: one ICF node per department or application area, each with its own `S_ICF` value and its own handler class calling `z2ui5_cl_ui5_http_handler=>run( )`, decides who may reach that area at all; which app of the area a user may run, and which data it may show, is decided in the app class (below). A node per app does not scale and is not needed, because the class name a node starts is user input anyway.

For a check of your own beyond `S_ICF`, put it in the handler before it calls the framework:

```abap
CLASS z2ui5_cl_my_http_handler DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

ENDCLASS.

CLASS z2ui5_cl_my_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.

    " may this user use the apps behind this node at all?
    AUTHORITY-CHECK OBJECT `Z_APP_AUTH`
                    ID `APP` FIELD `SALES`.

    IF sy-subrc <> 0.
      server->response->set_status( code   = 403
                                    reason = `Forbidden` ).
      RETURN.
    ENDIF.

    z2ui5_cl_ui5_http_handler=>run( server ).
  ENDMETHOD.

ENDCLASS.
```

The example uses a custom authorization object `Z_APP_AUTH` with an `APP` field — define the object in transaction `SU21` and assign it to the matching roles on your system. Create one node per user group or department, each with a handler like this, to separate who may reach which group of apps.

::: warning Do not decide on `app_start` in the handler
It is tempting to read the class from the request - ``server->request->get_form_field( `app_start` )`` - and refuse the ones a user may not run. That does not work. `app_start` is a URL parameter of the first page request only: the roundtrips are POSTs to the service URI, the class travels in the body of the first one (from the launchpad and from the [embed control](/advanced/fiori) it never is in the URL at all), every roundtrip after the start carries no class name, and `nav_app_call( )` changes the app without the client naming it. A handler that refuses on that parameter either breaks every roundtrip or checks nothing. Which app may run is decided in the app.
:::

## App-Level
Whoever reaches the node can start any class that implements `z2ui5_if_app` - the class name comes from the browser, as the URL parameter, a launchpad parameter or the `app` property of the [embed control](/advanced/fiori). The framework checks the type, not the user: a class that does not implement the interface is refused before it is instantiated. So the decision which user may run which app belongs in the app class, the way a report checks before it selects. It holds however the app is started - its own page, the launchpad, a Fiori elements section, or `nav_app_call( )` from another app - and a transport carries the app and its guard together.

### Example: Authorization Check in an App Class
In this approach, each app checks the user's permissions, like traditional ABAP apps.

<!-- playground: no Run button — an authority check needs a user and roles, and the browser has neither -->
```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_status TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      " Run an authorization check before showing anything
      AUTHORITY-CHECK OBJECT `Z_APP_AUTH`
                      ID `APP` FIELD `Z2UI5_APP_001`.

      IF sy-subrc <> 0.
        " Refuse, and SAY so - a blank screen looks like a broken app
        mv_status = `You are not authorized to use this app.`.
        client->message_box_display( text  = mv_status
                                     type  = `error`
                                     title = `Not authorized` ).
      ELSE.
        mv_status = |Authorized as { sy-uname }|.
      ENDIF.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `Authorization`

                  )->tag( `Text`
                      )->a( n = `text` v = client->_bind( mv_status ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

The refusal branch matters as much as the check. Returning without displaying
anything leaves the user on an empty frame with nothing to go on — indistinguishable
from an app that crashed. Say what happened, then return.

## Startup Parameters Are User Input
An app started with parameters - from the launchpad, or from a Fiori elements object page with the [embed control](/advanced/fiori) (`client->get( )-t_comp_params`) - can be started with any other value. An app that shows one sales order checks that the user may see *that* order, not only that they may run the app: the authorization for its sales organization, for example. Reading through a CDS view entity with an access control applies that access control, and EML on a RAP business object runs its authorization control; a plain `SELECT` on the database table checks nothing.
