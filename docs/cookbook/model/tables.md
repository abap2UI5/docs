---
outline: [2, 4]
---
# Tables
This section walks through rendering tabular and nested data in views.

### Basic Table
The example below binds a simple table to a UI5 control:
```abap
CLASS z2ui5_cl_sample_tab DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_row,
        count      TYPE i,
        value      TYPE string,
        descr      TYPE string,
      END OF ty_row.
    DATA mt_itab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_tab IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DO 100 TIMES.
        INSERT VALUE #(
          count = sy-index
          value = `red`
          descr = `this is a description` ) INTO TABLE mt_itab.
      ENDDO.

      DATA(tab) = z2ui5_cl_xml_view=>factory( )->page(
          )->table( client->_bind( mt_itab ) ).
      tab->columns(
          )->column( )->text( `Count` )->get_parent(
          )->column( )->text( `Value` )->get_parent(
          )->column( )->text( `Description` ).
      tab->items( )->column_list_item( )->cells(
         )->text( `{COUNT}`
         )->text( `{VALUE}`
         )->text( `{DESCR}` ).
      client->view_display( tab->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

### Editable
To make a table editable, switch the binding to `_bind_edit`:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DO 100 TIMES.
        INSERT VALUE #(
          count = sy-index
          value = `red`
          descr = `this is a description` ) INTO TABLE mt_itab.
      ENDDO.

      DATA(tab) = z2ui5_cl_xml_view=>factory( )->page(
          )->table( client->_bind_edit( mt_itab ) ).
      tab->columns(
          )->column( )->text( `Count` )->get_parent(
          )->column( )->text( `Value` )->get_parent(
          )->column( )->text( `Description` ).
      tab->items( )->column_list_item( )->cells(
         )->input( `{COUNT}`
         )->input( `{VALUE}`
         )->input( `{DESCR}` ).
      client->view_display( tab->stringify( ) ).

    ENDIF.

  ENDMETHOD.
```

### Nested Structures
You can also bind nested structures — use `structure/component` as the binding path:
```abap
CLASS z2ui5_cl_sample_nested_structures DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_tab,
        product TYPE string,
        BEGIN OF s_details,
          create_date TYPE string,
          create_by   TYPE string,
        END OF s_details,
      END OF ty_s_tab.
    DATA mt_itab TYPE STANDARD TABLE OF ty_s_tab WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_nested_structures IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    mt_itab = VALUE #(
        ( product = `table` s_details = VALUE #( create_date = `01.01.2023` create_by = `Peter`  ) )
        ( product = `chair` s_details = VALUE #( create_date = `25.10.2022` create_by = `Frank`  ) )
        ( product = `sofa`  s_details = VALUE #( create_date = `12.03.2024` create_by = `George` ) ) ).

    DATA(tab) = z2ui5_cl_xml_view=>factory( )->table( client->_bind( mt_itab ) ).

    DATA(columns) = tab->columns( ).
    columns->column( )->text( text = `Product` ).
    columns->column( )->text( text = `Created at` ).
    columns->column( )->text( text = `By` ).

    DATA(cells) = tab->items( )->column_list_item( ).
    cells->text( `{PRODUCT}` ).
    cells->text( `{S_DETAILS/CREATE_DATE}` ).
    cells->text( `{S_DETAILS/CREATE_BY}` ).

    client->view_display( tab->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
