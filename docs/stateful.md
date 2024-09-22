## abap2UI5 in stateful mode

#### Handler Implemention

```abap

  DATA(lv_method) = server->request->get_method( ).
    IF lv_method = 'HEAD'.
      "terminate session, currently thats the only supported HEAD request
      server->set_session_stateful( stateful = if_http_server=>co_disabled ).
      RETURN.
    ENDIF.

    z2ui5_cl_http_handler=>main(
      EXPORTING
        body       = server->request->get_cdata( )
        config     = VALUE #( t_config = VALUE #( ( n = `src` v = `/sap/public/bc/ui5_ui5/resources/sap-ui-core.js` ) ) )
      IMPORTING
        attributes = DATA(attributes)
      RECEIVING
        result     = DATA(response_body) ).
    server->response->set_cdata( response_body ).
    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_status( code = 200 reason = `success` ).

    "transform cookie to header based contextid handling
    IF attributes-stateful-switched = abap_true.
      server->set_session_stateful( stateful = attributes-stateful-active ).
      IF server->request->get_header_field( 'sap-contextid-accept' ) = 'header'.
        server->response->get_cookie(
          EXPORTING
            name  = 'sap-contextid'
          IMPORTING
            value = DATA(lv_contextid) ).
        IF lv_contextid IS NOT INITIAL.
          server->response->delete_cookie( 'sap-contextid' ).
          server->response->set_header_field( name = 'sap-contextid' value = lv_contextid ).
        ENDIF.
      ENDIF.
    ELSE.
      lv_contextid = server->request->get_header_field( 'sap-contextid' ).
      IF lv_contextid IS NOT INITIAL.
        server->response->set_header_field( name = 'sap-contextid' value = lv_contextid ).
      ENDIF.
    ENDIF.

```
