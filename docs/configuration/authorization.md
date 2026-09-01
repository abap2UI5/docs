---
outline: [2, 4]
---
# Authorization

abap2UI5 gives you flexibility in managing authorization. It has no built-in authorization mechanism, so you can build your own solution at either the service or the app level.

## Service-Level
The easiest way to manage access to different apps is to add authorization checks in the HTTP handler. This approach restricts access to individual apps based on the `app_start` URL parameter, directly in the ICF service handler class.

### Example: Restricting Access Based on URL Parameters
In this example, we use the ICF handler class to control which apps users can access. The ``get_form_field( `app_start` )`` call reads the `app_start` URL parameter that names the abap2UI5 app class to launch (e.g. `...?app_start=my_app`). If the user requests an unauthorized app, the handler refuses access.
```abap
CLASS z2ui5_cl_my_http_handler DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

ENDCLASS.

CLASS z2ui5_cl_my_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    " Read the app name from the request
    DATA(lv_app) = to_upper( server->request->get_form_field( `app_start` ) ).

    " Restrict access to a specific app
    IF lv_app <> `MY_APP`.
      RETURN.
    ENDIF.

    " Call the abap2UI5 handler
    z2ui5_cl_ui5_http_handler=>run( server ).
  ENDMETHOD.

ENDCLASS.
```
### Example: Authorization Objects in Service Handlers
You can also pair this with SAP authorization objects. The example below uses a custom authorization object `Z_APP_AUTH` with an `APP` field — define the object in transaction `SU21` and assign it to the matching roles on your system:
```abap
CLASS z2ui5_cl_my_http_handler DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

ENDCLASS.

CLASS z2ui5_cl_my_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    " Read the app name from the request
    DATA(lv_app) = to_upper( server->request->get_form_field( `app_start` ) ).

    " Run an authorization check
    AUTHORITY-CHECK OBJECT `Z_APP_AUTH`
                    ID `APP` FIELD lv_app.

    IF sy-subrc <> 0.
      " Authorization failed, refuse access
      RETURN.
    ENDIF.

    " Call the abap2UI5 handler if authorized
    z2ui5_cl_ui5_http_handler=>run( server ).
  ENDMETHOD.
ENDCLASS.
```
Create multiple HTTP endpoints for different users or departments to fine-tune access.

## App-Level
Alternatively, handle authorization within individual app classes. This approach works well when you want each app to check user permissions before acting.

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
                      )->a( n = `text` v = client->_bind( mv_status ) ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

The refusal branch matters as much as the check. Returning without displaying
anything leaves the user on an empty frame with nothing to go on — indistinguishable
from an app that crashed. Say what happened, then return.

::: warning
If you don't add authorization checks at the app level, make sure users can't bypass service-level checks by navigating between apps.
:::
