---
outline: [2, 4]
---
# Snippets

Copy-paste starting points for the most common app shapes. Each snippet is a complete, self-contained class — drop it in, activate it, and launch it by its name from the abap2UI5 landing page.

## Basic App Structure

The skeleton every abap2UI5 app starts from: implement `z2ui5_if_app`, dispatch on lifecycle phases with `CASE abap_true`, build the view in `check_on_init`, react to user actions in `check_on_event`.

```abap
CLASS z2ui5_cl_app_skeleton DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

ENDCLASS.

CLASS z2ui5_cl_app_skeleton IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        DATA(view) = z2ui5_cl_xml_view=>factory(
          )->page( `My App`
          )->text( `Hello World`
          )->button( text  = `Go`
                     press = client->_event( `GO` ) ).
        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `GO` ).
        client->message_box_display( `Button pressed!` ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

## Selection Screen

A classic input form: a few fields bound with `_bind_edit`, a button that triggers backend logic, results shown after submission.

```abap
CLASS z2ui5_cl_app_selection DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA carrid   TYPE string.
    DATA connid   TYPE string.
    DATA fldate   TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_selection IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        DATA(page) = z2ui5_cl_xml_view=>factory( )->page( `Selection Screen` ).

        page->simple_form( title = `Selection Criteria` editable = abap_true
            )->content( ns = `form`
                )->label( `Carrier ID`
                )->input( client->_bind_edit( carrid )
                )->label( `Connection ID`
                )->input( client->_bind_edit( connid )
                )->label( `Flight Date`
                )->date_picker( client->_bind_edit( fldate ) ).

        page->footer( )->overflow_toolbar(
            )->toolbar_spacer(
            )->button( text  = `Execute`
                       type  = `Emphasized`
                       press = client->_event( `EXECUTE` ) ).

        client->view_display( page->stringify( ) ).

      WHEN client->check_on_event( `EXECUTE` ).
        " run the search with carrid / connid / fldate
        client->message_box_display(
            |Executing with { carrid } / { connid } / { fldate }| ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

## Write Output

The quickest way to surface ABAP data on screen: build the HTML with `cl_demo_output=>get( )` and render it with the UI5 `html` control. Useful for prototypes and porting demo programs.

See [Demo Output](/cookbook/expert_more/demo_output) for the full CSS block. The minimal version:

```abap
CLASS z2ui5_cl_app_write_output DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA html TYPE string.

ENDCLASS.

CLASS z2ui5_cl_app_write_output IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      cl_demo_output=>begin_section( `My Report` ).
      cl_demo_output=>write_data( sy-uname ).
      cl_demo_output=>write_data( sy-datum ).
      html = cl_demo_output=>get( ).

      DATA(view) = z2ui5_cl_xml_view=>factory( )->page( `Write Output` ).
      view->html( content = client->_bind( html ) ).
      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Basic Table

A read-only table bound to an internal table, with three columns and one cell template per column.

```abap
CLASS z2ui5_cl_app_table_basic DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_row,
        id    TYPE i,
        name  TYPE string,
        descr TYPE string,
      END OF ty_row.
    DATA rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_app_table_basic IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DO 20 TIMES.
        INSERT VALUE #( id    = sy-index
                        name  = |Item { sy-index }|
                        descr = `Sample row` ) INTO TABLE rows.
      ENDDO.

      DATA(view) = z2ui5_cl_xml_view=>factory( )->page( `Basic Table` ).
      DATA(tab)  = view->table( client->_bind( rows ) ).
      tab->columns(
          )->column( )->text( `ID` )->get_parent(
          )->column( )->text( `Name` )->get_parent(
          )->column( )->text( `Description` ).
      tab->items( )->column_list_item( )->cells(
          )->text( `{ID}`
          )->text( `{NAME}`
          )->text( `{DESCR}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Table with Filtering, Sorting

Enable UI5's built-in column sorting and filtering by setting `sortProperty` and `filterProperty` on each column header. The `p13n` settings expose the personalization icon.

```abap
CLASS z2ui5_cl_app_table_p13n DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_row,
        id     TYPE i,
        name   TYPE string,
        status TYPE string,
      END OF ty_row.
    DATA rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_app_table_p13n IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DO 30 TIMES.
        INSERT VALUE #(
            id     = sy-index
            name   = |Item { sy-index }|
            status = COND #( WHEN sy-index MOD 2 = 0 THEN `open` ELSE `closed` )
          ) INTO TABLE rows.
      ENDDO.

      DATA(view) = z2ui5_cl_xml_view=>factory( )->page( `Sortable / Filterable Table` ).

      DATA(tab) = view->table(
          items                = client->_bind( rows )
          growing              = abap_true
          growing_threshold    = `10`
          mode                 = `MultiSelect`
          sticky               = `ColumnHeaders` ).

      tab->columns(
          )->column( sort_property = `ID`     filter_property = `ID`
              )->text( `ID` )->get_parent(
          )->column( sort_property = `NAME`   filter_property = `NAME`
              )->text( `Name` )->get_parent(
          )->column( sort_property = `STATUS` filter_property = `STATUS`
              )->text( `Status` ).

      tab->items( )->column_list_item( )->cells(
          )->text( `{ID}`
          )->text( `{NAME}`
          )->text( `{STATUS}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

For an interactive personalization dialog (column visibility, multi-sort, grouped filters), pair the table with `sap.m.p13n.Engine` or wrap it in a `SmartTable` — see the [UI5 SDK](https://sapui5.hana.ondemand.com) for the full feature set.
