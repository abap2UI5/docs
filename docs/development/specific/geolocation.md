# Geoloaction, Maps

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
You can visualize the geolocation with the UI5 Map Container:
```abap
METHOD z2ui5_if_app~main.
        view = z2ui5_cl_xml_view=>factory( ).
        client->view_display( view->shell(
              )->page(
                      title          = 'abap2UI5 - Device Capabilities'
                      navbuttonpress = client->_event( val = 'BACK' )
                      shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
                  )->_z2ui5( )->geolocation(
                                            finished         = client->_event( )
                                            longitude        = client->_bind_edit( longitude )
                                            latitude         = client->_bind_edit( latitude )
                                            altitude         = client->_bind_edit( altitude )
                                            altitudeaccuracy = client->_bind_edit( altitudeaccuracy )
                                            accuracy         = client->_bind_edit( accuracy )
                                            speed            = client->_bind_edit( speed )
                  )->simple_form( title    = 'Geolocation'
                                  editable = abap_true
                      )->content( 'form'
                          )->label( 'Longitude'
                          )->input( client->_bind_edit( longitude )
                          )->label( `Latitude`
                          )->input( client->_bind_edit( latitude )
                          )->label( `Altitude`
                          )->input( client->_bind_edit( altitude )
                          )->label( `Accuracy`
                          )->input( client->_bind_edit( accuracy )
                          )->label( `AltitudeAccuracy`
                          )->input( client->_bind_edit( altitudeaccuracy )
                          )->label( `Speed`
                          )->input( client->_bind_edit( speed )
                          )->label( `MapContainer`
                          )->button( text  = `Display`
                                     press = client->_event( `MAP_CONTAINER_DISPLAY` )
               )->get_parent( )->get_parent(
               )->map_container( autoadjustheight = abap_true
                    )->content( ns = `vk`
                        )->container_content(
                          title = `Analytic Map`
                          icon  = `sap-icon://geographic-bubble-chart`
                            )->content( ns = `vk`
                                )->analytic_map(
                                  initialposition = `9.933573;50;0`
                                  initialzoom     = `6`
                                )->vos(
                                    )->spots( client->_bind( mt_spot )
                                    )->spot(
                                      position      = `{POS}`
                                      contentoffset = `{CONTENTOFFSET}`
                                      type          = `{TYPE}`
                                      scale         = `{SCALE}`
                                      tooltip       = `{TOOLTIP}`
               )->stringify( ) ).
  ENDMETHOD.
```
