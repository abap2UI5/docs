---
outline: [2, 4]
---
# Authorization

abap2UI5 gives you flexibility in managing authorization. It has no built-in authorization mechanism, so you can build your own solution at either the service or the app level.

### Service-Level
An easy way to manage access to different apps: add checks in the HTTP handler. This approach restricts access to individual apps based on the APP_START parameter, directly in the ICF service handler class.

#### Example: Restricting Access Based on URL Parameters
In this example, we use the ICF handler class to control which apps users can access, based on the APP_START parameter in the HTTP request. The `get_header_field( 'APP_START' )` method reads the URL query parameter that names the abap2UI5 app class to launch. If the user requests an unauthorized app, the handler refuses access.
```abap
CLASS z2ui5_cl_launchpad_handler DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

ENDCLASS.

CLASS z2ui5_cl_launchpad_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    " Read the app name from the request
    DATA(lv_app) = server->request->get_header_field( `APP_START` ).

    " Restrict access to a specific app
    IF lv_app <> `MY_APP`.
      RETURN.
    ENDIF.

    " Call the abap2UI5 handler
    z2ui5_cl_http_handler=>run( server ).
  ENDMETHOD.

ENDCLASS.
```
#### Example: Authorization Objects in Service Handlers
You can also pair this with SAP authorization objects. The example below uses a custom authorization object `Z_APP_AUTH` with an `APP` field — define the object in transaction `SU21` and assign it to the relevant roles on your system:
```abap
CLASS z2ui5_cl_launchpad_handler DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

ENDCLASS.

CLASS z2ui5_cl_launchpad_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    " Read the app name from the request
    DATA(lv_app) = server->request->get_header_field( `APP_START` ).

    " Run an authorization check
    AUTHORITY-CHECK OBJECT `Z_APP_AUTH`
                    ID `APP` FIELD lv_app.

    IF sy-subrc <> 0.
      " Authorization failed, refuse access
      RETURN.
    ENDIF.

    " Call the abap2UI5 handler if authorized
    z2ui5_cl_http_handler=>run( server ).
  ENDMETHOD.
ENDCLASS.
```
Create multiple HTTP endpoints for different users or departments to fine-tune access.

### App-Level
Alternatively, handle authorization within individual app classes. This approach works well when you want each app to check user permissions before acting.

#### Example: Authorization Check in an App Class
In this approach, each app checks the user's permissions, like traditional ABAP apps.

```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    " Run an authorization check before launching the app
    AUTHORITY-CHECK OBJECT `Z_APP_AUTH`
                    ID `APP` FIELD `Z2UI5_APP_001`.

    IF sy-subrc <> 0.
      " Authorization failed, refuse access
      RETURN.
    ENDIF.

    " Continue with app processing if authorized
    " (App logic goes here)
  ENDMETHOD.

ENDCLASS.
```

::: warning
If you don't add authorization checks at the app level, make sure users can't bypass service-level checks by navigating between apps.
:::
