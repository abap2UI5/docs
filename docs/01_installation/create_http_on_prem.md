### Create On-Premise HTTP Handler

Go to se80, create a new package and a new class:<br>
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/cd3cf92a-2999-4194-89cc-0d9cb88d4aa7"><br><br>
Open the source-code based view:<br>
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/8e82279b-5fcc-44ad-a240-f473da83e847"><br><br>
Add the interface and code snippet and activate the class:<br>
<img width="800" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/86e43203-0563-47fc-be0b-3c119cdc6425"><br><br>

```ABAP
CLASS zcl_my_abap2ui5_http_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_my_abap2ui5_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.

    DATA(lv_resp) = SWITCH #( server->request->get_method( )
       WHEN 'GET'  THEN z2ui5_cl_http_handler=>http_get( )
       WHEN 'POST' THEN z2ui5_cl_http_handler=>http_post( server->request->get_cdata( ) ) ).

    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_cdata( lv_resp ).
    server->response->set_status( code = 200 reason = `success` ).

  ENDMETHOD.
ENDCLASS.
```
<br><br>
Next go to transaction SICF:<br>
<img width="736" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/1fff1ed9-b7a4-4713-842d-2736ae3a59a8"><br><br>
Create at you favorite position a new node:<br>
<img width="799" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/1539407f-73d9-478d-8342-3e374aa4b5be"><br><br>
Accept this:<br>
<img width="671" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/5db7edb1-c781-4c84-a10f-07f089a48689"><br><br>
and:<br>
<img width="702" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/0e23822d-1588-4a04-83d1-461feb792909"><br><br>
<img width="777" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/336fd0bf-6d3e-4a3d-ab38-59756caeb399"><br><br>
Save and choose your package:<br>
<img width="552" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/6d3599a4-f04a-4eb1-81c1-5e99da5f361c"><br><br>

Go back and activate the service:<br>
<img width="746" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/59e7699d-8add-4065-9f48-63b4099beb8f"><br><br>
Now test the service:<br>
<img width="579" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/a4a1746f-3bae-47dc-a6a9-244610480c33"><br><br>
You should see the starting page now:<br>
<img width="1270" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/d72be34e-9873-43db-821f-0039719e6647"><br><br>
