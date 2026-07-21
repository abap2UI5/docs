---
outline: [2, 4]
---
# Trees
For hierarchical data, abap2UI5 uses nested ABAP structures to represent tree levels. Each level holds a table of child nodes, which UI5 traverses to build the expandable tree control.

### Tree
Define a type hierarchy where each node contains a child table of the next level:
```abap
CLASS z2ui5_cl_sample_tree DEFINITION PUBLIC.

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
        )->tree( items = client->_bind( prodh_nodes )
            )->items( )->standard_tree_item(
                selected = `{IS_SELECTED}`
                title    = `{TEXT}` ).

    client->view_display( tree->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

The child table field (`nodes` in the example above) is the key: UI5 follows that field name to locate sub-items at each level. The name must match across all levels but can be anything you choose.

Note that the example binds `IS_SELECTED` to an editable control so the user's selection is synced back to ABAP. A display-only tree needs no editable cells.
