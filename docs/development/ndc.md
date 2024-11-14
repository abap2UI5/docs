---
outline: [2, 4]
---
# Native Device Capabilities

Modern browsers and HTML make it easy to access native device capabilities directly. For more details and examples, check out the blog post Exploring External Libraries & Native Device Capabilities with abap2UI5 [here.](https://community.sap.com/t5/technology-blogs-by-members/abap2ui5-10-extensions-i-exploring-external-libraries-native-device/ba-p/13576910)

#### Geolocation
This example demonstrates how to retrieve geolocation data such as longitude, latitude, altitude, and speed. Also see `Z2UI5_CL_DEMO_APP_120`.

```abap
CLASS z2ui5_cl_sample_geolocation DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app .

    DATA longitude TYPE string.
    DATA latitude TYPE string.
    DATA altitude TYPE string.
    DATA speed TYPE string.
    DATA altitudeaccuracy TYPE string.
    DATA accuracy TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_geolocation IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

       DATA(view) = z2ui5_cl_xml_view=>factory( ).
        client->view_display( view->shell(
            )->page( )->_z2ui5( 
                )->geolocation(
                    finished         = client->_event( `POST` )
                    longitude        = client->_bind_edit( longitude )
                    latitude         = client->_bind_edit( latitude )
                    altitude         = client->_bind_edit( altitude )
                    altitudeaccuracy = client->_bind_edit( altitudeaccuracy )
                    accuracy         = client->_bind_edit( accuracy )
                    speed            = client->_bind_edit( speed )
            )->stringify( ) ).

    CASE client->get( )-event.
      WHEN 'POST'.
        "processing...
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

#### Device Information
This example demonstrates how to retrieve detailed device information such as UI5 version, device type, OS, browser, and screen dimensions. Also see `z2ui5_cl_demo_app_122`.
```abap
CLASS z2ui5_cl_sample_device DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA ui5_version       TYPE string.
    DATA ui5_theme         TYPE string.
    DATA ui5_gav           TYPE string.
    DATA device_systemtype TYPE string.
    DATA device_os         TYPE string.
    DATA device_browser    TYPE string.
    DATA check_initialized TYPE abap_bool.
    DATA device_phone      TYPE abap_bool.
    DATA device_desktop    TYPE abap_bool.
    DATA device_tablet     TYPE abap_bool.
    DATA device_combi      TYPE abap_bool.
    DATA device_height     TYPE string.
    DATA device_width      TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_device IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    DATA(lo_view) = z2ui5_cl_xml_view=>factory(
        )->page( )->_z2ui5( 
            )->info_frontend( 
                finished          = client->_event( `POST` )
                device_browser    = client->_bind_edit( device_browser )
                device_os         = client->_bind_edit( device_os )
                device_systemtype = client->_bind_edit( device_systemtype )
                ui5_gav           = client->_bind_edit( ui5_gav )
                ui5_theme         = client->_bind_edit( ui5_theme )
                ui5_version       = client->_bind_edit( ui5_version )
                device_phone      = client->_bind_edit( device_phone   )
                device_desktop    = client->_bind_edit( device_desktop )
                device_tablet     = client->_bind_edit( device_tablet  )
                device_combi      = client->_bind_edit( device_combi   )
                device_height     = client->_bind_edit( device_height  )
                device_width      = client->_bind_edit( device_width   ) ).

    client->view_display( lo_view->stringify( ) ).

    IF client->get( )-event = 'POST'.
      client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack  ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

#### Camera
For camera capabilities, refer to the addon section under [js-libraries.](/addons/ext_js)


::: tip **UI5 Versions**
Depending on browser permissions and configuration settings, access to or use of native device capabilities (NDC) may be restricted. Watch out for any warning messages directly from your browser during testing.
:::