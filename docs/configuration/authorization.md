---
outline: [2, 4]
---
# Authorization

abap2UI5 offers flexible ways to manage authorization handling. It doesn’t include a built-in authorization mechanism, allowing developers to create their own solutions either at the service or application level.

### Service-Level 
One of the easiest ways to manage access to different apps is by implementing checks within the HTTP handler. This approach allows the developer to restrict access to individual apps based on the APP_START parameter, directly in the ICF service handler class.

##### Example: Restricting Access Based on URL Parameters
In this example, we use the ICF handler class to control which apps can be accessed based on the APP_START parameter in the HTTP request. If an unauthorized app is requested, access is denied.
```abap
CLASS z2ui5_cl_launchpad_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_launchpad_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    " Read the app name from the request
    DATA(lv_app) = server->request->get_header_field( 'APP_START' ).
    
    " Restrict access to a specific app
    IF lv_app <> 'MY_APP'.
      RETURN.
    ENDIF.
    
    " Run the abap2UI5 handler
    z2ui5_cl_http_handler=>run( server ).
  ENDMETHOD.

ENDCLASS.
```
##### Using Authorization Objects in Service Handlers
You can also use the SAP authorization objects:
```abap
CLASS z2ui5_cl_launchpad_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_launchpad_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    " Read the app name from the request
    DATA(lv_app) = server->request->get_header_field( 'APP_START' ).
    
    " Perform an authorization check
    AUTHORITY-CHECK OBJECT 'Z_APP_AUTH'
                    ID 'APP' FIELD lv_app.

    IF sy-subrc <> 0.
      " Authorization failed, deny access
      RETURN.
    ENDIF.

    " Run the abap2UI5 handler if authorized
    z2ui5_cl_http_handler=>run( server ).
  ENDMETHOD.
ENDCLASS.
```

### Application-Level
Alternatively, you can handle authorization within individual app classes. This approach is useful if you want to delegate authorization to each app, ensuring that it checks user permissions before performing any actions.

##### Example: Authorization Check in an App Class
In this method, each app is responsible for checking the user’s permissions, similar to how it's done in traditional SAP ABAP applications.
abap
```abap
CLASS z2ui5_cl_demo_app_001 DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_demo_app_001 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    " Perform an authorization check before launching the app
    AUTHORITY-CHECK OBJECT 'Z_APP_AUTH'
                    ID 'APP' FIELD 'Z2UI5_APP_001'.

    IF sy-subrc <> 0.
      " Authorization failed, deny access
      RETURN.
    ENDIF.

    " Continue with app processing if authorized
    " (App logic goes here)
  ENDMETHOD.

ENDCLASS.
```

::: tip Information
 If you don't implement authorization checks at the app level, make sure that end users cannot bypass service-level authorization checks by navigating between apps.
:::