# DX: ALV & Selection-Screen Style?


hello world small app


```abap
CLASS zcl_app_adt DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

ENDCLASS.


CLASS zcl_app_adt IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    out->write( `Hello World` ).
  ENDMETHOD.

ENDCLASS.
```

```abap
CLASS zcl_app_ui5 DEFINITION PUBLIC CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.


CLASS zcl_app_ui5 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    client->view_display( z2ui5_cl_xml_view=>factory( )->text( `Hello World` ) ).
  ENDMETHOD.

ENDCLASS.
```


##### 9. One HTTP-Service for all Apps

First, we do not define a specific HTTP-Service for transmitting the View and the Data. Instead, every app uses the same generic HTTP-Handler including two strings (one for the View and one for the Data) eliminating the need to develop individual OData-Services with SEGW or CDS. During runtime the ABAP variables & tables are transformed into a JSON-Model and transmitted as a string to the frontend. In JavaScript it is parsed again into a JSON-Model and binded to the UI5-View:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/163ca12b-fe37-43e8-80b6-a5eaae703d69" />

Data Transfer in abap2UI5 - ABAP variables & tables are automatically synchronized with the UI5-Model

Furthermore we not only send the data but also the metadata (Data Model) with every request (7). This is different from classic OData communication, where the metadata is sent with the initial OData request to establish the model at the beginning, and only the data is exchanged afterward. With this approach, we can now send different models with every request:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/95fe59c3-7e8a-4e21-8690-12de1110779f" />

OData vs. UI5 Over the Wire - Model & Data transfer


##### 13. HTTP-Service

All apps and data models use the same single generic HTTP-Handler, which can be observed by setting a breakpoint in your app and examining the call stack.

<img width="600" alt="image" src="https://github.com/user-attachments/assets/1ce80652-4105-4ee5-84e8-35a87eb47556" />

Call stack of an abap2UI5 app

Every app implementation is a REST-based HTTP-Post implementation, in which no session is maintained between two requests.

##### 23. As simple as possible

So, we have seen in (22), apps can be made very complex, but the opposite is also possible - we can make them extremely simple. One beautifully minimalistic approach is the use of if_oo_adt_classrun. By implementing just one method, we can generate an output with a single click (F9). This is extremely efficient and was one of the role models for abap2UI5. Here's a comparison of both approaches:

<img width="600" alt="image" src="https://github.com/user-attachments/assets/28a09830-ba3a-4608-aab9-5f4af8028a18" />

if_oo_adt_classrun vs. abap2UI5

To summarize what we have covered so far, abap2UI5 is built in a highly generic manner, placing most of the responsibility on the user's apps. As a result, we gain a a lot of flexibility and freedom in the app implementation, but we also have full responsibility for the view creation and the program flow. Furthermore we have to keep the following downsides in mind.
