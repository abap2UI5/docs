# DX: Let's keep it simple?

Developer Experience (DX) refers to the overall experience developers have when interacting with tools, processes, and systems during software development. There are a lot of different frameworks lets dive into the most beautiful one and see how these idea got adapten into abap2UI5.

### if_oo_adt_classrun

What is the easiest way to output data in an abap stack, it has to be both abao cloud ready and abap standard? its `if_oo_adt_classrun` the best way.

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
it has the following advantages:
* single file for a whole app
* class based
* abap cloud ready
* 100% abapGit compatible which macke ist

this was excatly what the abap2ui app keep to mimic, an abap2ui5 app with the outout above looks as folows:

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
additionaly it solves the following problems:
* np need to install adt, the output comes in a browser
* end user can use it, becaue it published
* at the fotnend it cretaes a ui5 app following offical sap fiori user experience

so this is the basic, from here we try to add more functionality.


### Selection Screens
as a last prerequste we need some input. also the easiest way are selection screen, lets remember how that went:
```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT 'MDG_TR'.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| type 'I'.
```
this is thew wy we can do it with abap2ui5:
```abap
"app
"app
"app
```

### ALV

what is the easiest way to out out tables? its the goold als cl_salv_table:
```abap
REPORT zre_app_alv.

SELECT FROM t100
 FIELDS *
 INTO TABLE @DATA(gt_t100)
 UP TO 10 ROWS.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table   = DATA(go_salv)
  CHANGING
    t_table        = gt_t100 ).
go_salv->display( ).
```
15 lines of code, a single file, and the snippet is ready to use to transport to production for the use of end users. this is strong! unfortunately it is not cloud ready and therefor not future. let mix this with the baap2ui5 approach from above. a abap2ui5 we can create somethig quite similar with the follwoing snippet:
```abap
"app
"app
"app
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
