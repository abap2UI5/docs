CLASS zcl_my_handler_cloud DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_my_handler_cloud IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

   response->set_text( z2ui5_cl_http_handler=>main( request->get_text( ) ) ).
   response->set_header_field( i_name = `cache-control` i_value = `no-cache` ).
   response->set_status( 200 ).

  ENDMETHOD.

ENDCLASS.
