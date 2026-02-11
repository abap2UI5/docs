# Tables, Trees
This section covers how to display complex data structures like tables, trees, and nested structures in your views.

### Tables 
The example below demonstrates how to bind a simple table to a UI5 control:
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
 
    DO 100 TIMES.
      INSERT VALUE #(
        count = sy-index
        value = `red`
        descr = `this is a description` ) INTO TABLE mt_itab.
    ENDDO.
 
    DATA(tab) = z2ui5_cl_xml_view=>factory( )->page(
        )->table( client->_bind( mt_itab ) ).
    tab->columns(
        )->column( )->text( `Counter` )->get_parent(
        )->column( )->text( `Value` )->get_parent(
        )->column( )->text( `Description` ).
    tab->items( )->column_list_item( )->cells(
       )->text( `{COUNT}`
       )->text( `{VALUE}`
       )->text( `{DESCR}` ).
    client->view_display( tab->stringify( ) ).
 
  ENDMETHOD.
ENDCLASS.
```

### Editable
Making a table editable is a simple change. You just need to switch the binding mode to `bind_edit` :
```abap
  METHOD z2ui5_if_app~main.
 
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
       )->text( `{COUNT}`
       )->text( `{VALUE}`
       )->text( `{DESCR}` ).
    client->view_display( tab->stringify( ) ).
 
  ENDMETHOD.
```

### Tree
To work with trees, you need to use nested structures. Here is an example:
```abap
CLASS z2ui5_cl_sample_tree DEFINITION PUBLIC CREATE PUBLIC.
 
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_prodh_node_level3,
        is_selected TYPE abap_bool,
        text        TYPE string,
        prodh       TYPE string,
      END OF ty_prodh_node_level3,
      BEGIN OF ty_prodh_node_level2,
        is_selected TYPE abap_bool,
        text        TYPE string,
        prodh       TYPE string,
        nodes       TYPE STANDARD TABLE OF ty_prodh_node_level3 WITH DEFAULT KEY,
      END OF ty_prodh_node_level2,
      BEGIN OF ty_prodh_node_level1,
        is_selected TYPE abap_bool,
        text        TYPE string,
        prodh       TYPE string,
        nodes       TYPE STANDARD TABLE OF ty_prodh_node_level2 WITH DEFAULT KEY,
      END OF ty_prodh_node_level1,
      ty_prodh_nodes TYPE STANDARD TABLE OF ty_prodh_node_level1 WITH DEFAULT KEY.
    DATA prodh_nodes    TYPE ty_prodh_nodes.

ENDCLASS.

CLASS z2ui5_cl_sample_tree IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
  
    prodh_nodes = VALUE #( (
        text = `Machines`
        prodh  = `00100`
        nodes  = VALUE #( (
            text = `Pumps`
            prodh = `0010000100`
            nodes = VALUE #( (
                text  = `Pump 001`
                prodh = `001000010000000100` ) (
                text  = `Pump 002`
                prodh = `001000010000000105` ) )
        ) ) ) (
        text  = `Paints`
        prodh = `00110`
        nodes = VALUE #( (
            text  = `Gloss paints`
            prodh = `0011000105`
            nodes = VALUE #( (
                text  = `Paint 001`
                prodh = `001100010500000100` ) (
                text  = `Paint 002`
                prodh = `001100010500000105` )
    ) ) ) ) ).

    DATA(tree) = z2ui5_cl_xml_view=>factory( )->page(
        )->tree( items = client->_bind_edit( prodh_nodes )
            )->items( )->standard_tree_item(
                selected = `{IS_SELECTED}`
                title    = `{TEXT}` ).
 
    client->view_display( tree->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Nested Structures
Binding nested structures is also possible. Use `structure/component` in the binding path, as shown below:
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

    DATA(lo_columns) = tab->columns( ).
    lo_columns->column( )->text( text = `Product` ).
    lo_columns->column( )->text( text = `Created at` ).
    lo_columns->column( )->text( text = `By` ).

    DATA(lo_cells) = tab->items( )->column_list_item( ).
    lo_cells->text( `{PRODUCT}` ).
    lo_cells->text( `{S_DETAILS/CREATE_DATE}` ).
    lo_cells->text( `{S_DETAILS/CREATE_BY}` ).

    client->view_display( tab->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```
