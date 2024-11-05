# Runtime Typed Data with S-RTTI

<i class="fa-brands fa-github"></i> [S-RTTI on GitHub](https://github.com/sandraros/S-RTTI)

In abap2UI5 musst du nicht nowendigerweise dein datenmodell zur designtime typisieren. Du kannst genuso gut auch mit generischen datenreferencen arbetien und zur laufzeit auf basis diene programmlogik iene typisierung vornehmen. Das ist zum Beispiel bei tabellen interessant um je nach user eingabe unterschiedliche spaleten anzuzeigen.

abap2UI5 serializes your app instances to ensure stateless behavior in client communication. Unfortunately, the sap standard transformation features are limited, for example not supporting local types created at runtime. Fortunately the project S-RTTI fills this gap. 


#### Normal

```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_row,
        title TYPE string,
        value TYPE string,
        descr TYPE string,
      END OF ty_row.
    DATA mt_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    mt_tab = VALUE #(
        ( title = 'entry 01'  value = 'red'  descr = 'this is a description' )
        ( title = 'entry 02'  value = 'blue'  descr = 'this is a description' )
        ( title = 'entry 02'  value = 'gray'  descr = 'this is a description' ) ).

    client->message_box_display( `this works without problems` ).

  ENDMETHOD.
ENDCLASS.
```

#### Generic Data Reference (local type)
```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.
    DATA mr_tab TYPE REF TO data.

ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    TYPES:
      BEGIN OF ty_row,
        title TYPE string,
        value TYPE string,
        descr TYPE string,
      END OF ty_row.
    TYPES ty_t_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

    CREATE DATA mr_tab TYPE ty_T_tab.

    FIELD-SYMBOLS <tab> TYPE ty_T_tab.
    ASSIGN mr_tab->* TO <tab>.
    <tab> = VALUE #(
        ( title = 'entry 01'  value = 'red'  descr = 'this is a description' )
        ( title = 'entry 02'  value = 'blue' descr = 'this is a description' )
        ( title = 'entry 02'  value = 'gray' descr = 'this is a description' ) ).

    client->message_box_display( `this works only with S-RTTI` ).

  ENDMETHOD.
ENDCLASS.
```
