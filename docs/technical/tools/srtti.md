---
outline: [2, 4]
---
# S-RTTI

In abap2UI5, you don't always need to define your data model at design time. Instead, you can work with generic data references and apply typing dynamically at runtime based on your program logic. This is especially useful for cases like tables, where different columns and table types may appear based on user input.

abap2UI5 serializes app instances so client communication remains stateless. But SAP's standard transformation features are limited and don't support data references with local types created at runtime.

To work around this limitation, abap2UI5 integrates the [S-RTTI](https://github.com/sandraros/S-RTTI) project.

#### Standard Transformation
With fully typed data at design time, the standard SAP transformation works right away:

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

    CREATE DATA mr_tab TYPE ty_t_tab.

    FIELD-SYMBOLS <tab> TYPE ty_t_tab.
    ASSIGN mr_tab->* TO <tab>.
    <tab> = VALUE #(
        ( title = `entry 01`  value = `red`  descr = `this is a description` )
        ( title = `entry 02`  value = `blue` descr = `this is a description` ) ).

    client->message_box_display( `this works out of the box` ).

  ENDMETHOD.
ENDCLASS.
```

#### Transformation with S-RTTI
When working with data typed dynamically at runtime through local types, S-RTTI is needed:
```abap
CLASS z2ui5_cl_app DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mr_tab TYPE REF TO data.

ENDCLASS.

CLASS z2ui5_cl_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    DATA(o_struct_desc) = cl_abap_structdescr=>describe_by_name( `USR01` ).
    DATA(o_table_desc) = cl_abap_tabledescr=>create(
      p_line_type  = CAST #( o_struct_desc )
      p_table_kind = cl_abap_tabledescr=>tablekind_std
      p_unique     = abap_false ).

    CREATE DATA mr_tab TYPE HANDLE o_table_desc.

    client->message_box_display( `this works only with S-RTTI` ).

  ENDMETHOD.
ENDCLASS.
```

#### Functionality
With generic types, the standard transformation throws an error. abap2UI5 works around this by looping over all attributes:
- For each generic attribute, S-RTTI runs a separate serialization beforehand.
- The data goes into a separate table.
- The variable is reset, and the standard SAP transformation runs again.
- On deserialization, abap2UI5 recreates the object and restores the table content back into the attributes.

This approach delivers compatibility with dynamic types while keeping the transformation process reliable. It runs in the background, independent of the two sample app implementations above.

#### Integration

S-RTTI integrates directly into the `z2ui5` namespace and installs with abap2UI5 automatically. Use it anywhere in your code:
```abap
z2ui5_cl_srt_datadescr=>
```
#### Updates
Every S-RTTI update and bug fix flows automatically into abap2UI5 through GitHub Actions and the [mirror-srtti](https://github.com/abap2UI5/mirror-srtti) repository, so you always run the latest version.
