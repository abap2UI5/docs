# Tables, Trees
In this section, we will explore how to bind deep data models, such as tables and trees.

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
    DATA t_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

ENDCLASS.
 
CLASS z2ui5_cl_sample_tab IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
 
    DO 100 TIMES.
      DATA ls_row TYPE ty_row.
      ls_row-count = sy-index.
      ls_row-value = 'red'.
      ls_row-descr = 'this is a description'.
      INSERT ls_row INTO TABLE t_tab.
    ENDDO.
 
    DATA(tab) = z2ui5_cl_xml_view=>factory( )->page( )->table(
        items = client->_bind( t_tab ) ).
 
    tab->columns(
        )->column( )->text( 'Counter' )->get_parent(
        )->column( )->text( 'Value' )->get_parent(
        )->column( )->text( 'Description' ).
    tab->items( )->column_list_item( )->cells(
       )->text( '{COUNT}'
       )->text( '{VALUE}'
       )->text( '{DESCR}' ).
 
    client->view_display( view->stringify( ) ).
 
  ENDMETHOD.
ENDCLASS.
```

### Editable
Making a table editable is a simple change. You just need to switch the binding mode to `bind_edit` :
```abap
  METHOD z2ui5_if_app~main.
 
    DO 100 TIMES.
      DATA ls_row TYPE ty_row.
      ls_row-count = sy-index.
      ls_row-value = 'red'.
      ls_row-descr = 'this is a description'.
      INSERT ls_row INTO TABLE t_tab.
    ENDDO.
 
    DATA(tab) = z2ui5_cl_xml_view=>factory( )->page( )->table(
        items = client->_bind_edit( t_tab ) ).
    tab->columns(
        )->column( )->text( 'Count' )->get_parent(
        )->column( )->text( 'Value' )->get_parent(
        )->column( )->text( 'Description' ).
    tab->items( )->column_list_item( )->cells(
       )->text( '{COUNT}'
       )->text( '{VALUE}'
       )->text( '{DESCR}' ).
 
    client->view_display( view->stringify( ) ).
 
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
  
      prodh_nodes =
      VALUE #( ( text = 'Machines'
               prodh  = '00100'
               nodes  = VALUE #( ( text = 'Pumps'
                                  prodh = '0010000100'
                                  nodes = VALUE #( ( text  = 'Pump 001'
                                                     prodh = '001000010000000100' )
                                                   ( text  = 'Pump 002'
                                                     prodh = '001000010000000105' )
                                          )
                       ) )
             )
             ( text  = 'Paints'
               prodh = '00110'
               nodes = VALUE #( ( text  = 'Gloss paints'
                                  prodh = '0011000105'
                                  nodes = VALUE #( ( text  = 'Paint 001'
                                                     prodh = '001100010500000100' )
                                                   ( text  = 'Paint 002'
                                                     prodh = '001100010500000105' )
                                          )
                       ) )
             ) ).

    DATA(page) = z2ui5_cl_xml_view=>factory( )->page( ).
 
    page->tree( items = client->_bind_edit( prodh_nodes )
        )->items( )->standard_tree_item(
           selected = '{IS_SELECTED}'
           title    = '{TEXT}' ).
 
    client->view_display( page->stringify( ) ).

 ENDMETHOD.
ENDCLASS.
```
