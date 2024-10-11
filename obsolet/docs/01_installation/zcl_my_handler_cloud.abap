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
   z2ui5_cl_http_handler=>factory_cloud( req = request res = response )->main( ).
  ENDMETHOD.

ENDCLASS.