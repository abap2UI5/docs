CLASS zcl_my_handler_onprem DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_my_handler_onprem IMPLEMENTATION.

 METHOD if_http_extension~handle_request.

    z2ui5_cl_http_handler=>factory( server )->main( ).

  ENDMETHOD.

ENDCLASS.
