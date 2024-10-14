# RFC Connector

Find all information [here.](https://www.linkedin.com/pulse/calling-abap2ui5-apps-remotely-via-rfc-abap2ui5-btoue/?trackingId=x648I3DPaEwjw1bW9PNavg%3D%3D)


## abap2UI5 - RFC Connector

_Running into problems or found a bug? Create an issue [**here**](https://github.com/abap2UI5/abap2UI5/issues)_

### Approach
Remotely call abap2UI5 apps via RFC:
<img width="1420" alt="image" src="https://github.com/abap2UI5/abap2UI5-connector_rfc/assets/102328295/5787755c-f4f1-48d8-a9da-50b4f04db9ed">
<br>
[Link](https://excalidraw.com/#json=Z27bQMS9ZH-3xgMDxLZ1R,WfuSCgOsHoJr8e339WSgjA)

### Installation
Install this repository with [abapGit](https://abapgit.org) ![abapGit](https://docs.abapgit.org/img/favicon.png) on the system with your abap2UI5 apps. Install this handler on client system.
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

### More
Check out this [blog post](https://www.linkedin.com/pulse/calling-abap2ui5-apps-remotely-via-rfc-abap2ui5-btoue/?trackingId=BJWSE77kp0aJRwpREQpVrQ%3D%3D) on LinkedIn to learn more.
