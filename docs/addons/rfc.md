---
outline: [2, 6]
---
# Remote App Calls

<i class="fa-brands fa-github"></i> [Repository](https://github.com/abap2UI5-addons/rfc-connector)

Find all information in the blog article [here.](https://www.linkedin.com/pulse/calling-abap2ui5-apps-remotely-via-rfc-abap2ui5-btoue/?trackingId=x648I3DPaEwjw1bW9PNavg%3D%3D)

### Approach
Remotely call abap2UI5 apps via RFC:
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-connector_rfc/assets/102328295/5787755c-f4f1-48d8-a9da-50b4f04db9ed">
<br>

### Installation
Install this repository with abapGit on the system. Install this handler on client system.
Handler:
```abap
CLASS z2ui5_cl_rfc_connector_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_rfc_connector_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.

    DATA(lv_resp) = ``.
    CALL FUNCTION 'Z2UI5_FM_RFC_CONECTOR'
      DESTINATION 'NONE' "setup your destination here
      EXPORTING
        iv_method   = server->request->get_method( )
        iv_request  = server->request->get_cdata( )
      IMPORTING
        rv_response = lv_resp.

    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_cdata( lv_resp ).
    server->response->set_status( code = 200 reason = `success` ).

  ENDMETHOD.

ENDCLASS.
```
Setup destinations in SM50 that both systems can call each other and create an ICF Endpoint to call your abap2UI5 apps.