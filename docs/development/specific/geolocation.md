# Geolocation, Maps

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
      WHEN `POST`.
        "processing...
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```
