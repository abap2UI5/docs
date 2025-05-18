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
CLASS zcl_app_input DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA pa_arbgb TYPE string VALUE 'MDG_TR'.
ENDCLASS.

CLASS zcl_app_input IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).
      client->view_display(
        z2ui5_cl_xml_view=>factory(
            )->input( client->_bind_edit( pa_arbgb )
            )->button( text  = 'post' press = client->_event( 'POST' ) ) ).
    ELSEIF client->get( )-event = 'POST'.
      client->message_box_display( |Input: { pa_arbgb }| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
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
CLASS zcl_app_alv DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS zcl_app_alv IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    SELECT FROM t100
     FIELDS *
     INTO TABLE @DATA(gt_t100)
     UP TO 10 ROWS.

    DATA(tab) = z2ui5_cl_xml_view=>factory(
        )->table( client->_bind_local( gt_t100 ) ).

    DATA(lo_col) = tab->columns( ).
    lo_col->column( )->text( text = `SPRSL` ).
    lo_col->column( )->text( text = `ARBGB` ).
    lo_col->column( )->text( text = `MSGNR` ).
    lo_col->column( )->text( text = `TEXT` ).

    DATA(lo_cell) = tab->items( )->column_list_item( ).
    lo_cell->text( `{SPRSL}` ).
    lo_cell->text( `{ARBGB}` ).
    lo_cell->text( `{MSGNR}` ).
    lo_cell->text( `{TEXT}` ).

    client->view_display( tab ).

  ENDMETHOD.
ENDCLASS.
```



