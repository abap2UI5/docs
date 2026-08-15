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
        DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
                )->a( n = `xmlns`     v = `sap.m`
                )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

                )->ele( `Page`
                    )->a( n = `title` v = `My App`

                )->tag( `Text`
                    )->a( n = `text` v = `Hello World`
                )->tag( `Button`
                    )->a( n = `text`  v = `Go`
                    )->a( n = `press` v = client->_event( `GO` ) ).

        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `GO` ).
        client->message_box_display( `Button pressed!` ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

## Selection Screen

A classic input form: a few fields bound with `_bind`, a button that triggers backend logic, results shown after submission.

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
        DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
                )->a( n = `xmlns`     v = `sap.m`
                )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                )->a( n = `xmlns:form` v = `sap.ui.layout.form`

                )->ele( `Page`
                    )->a( n = `title` v = `Selection Screen`

                )->ele( n = `SimpleForm` ns = `form`
                    )->a( n = `title`    v = `Selection Criteria`
                    )->a( n = `editable` b = abap_true

                    )->ele( n = `content` ns = `form`
                        )->tag( `Label`
                            )->a( n = `text` v = `Carrier ID`
                        )->tag( `Input`
                            )->a( n = `value` v = client->_bind( carrid )
                        )->tag( `Label`
                            )->a( n = `text` v = `Connection ID`
                        )->tag( `Input`
                            )->a( n = `value` v = client->_bind( connid )
                        )->tag( `Label`
                            )->a( n = `text` v = `Flight Date`
                        )->tag( `DatePicker`
                            )->a( n = `value` v = client->_bind( fldate )

                )->end(
                )->end(

                )->ele( `footer`
                    )->ele( `OverflowToolbar`
                        )->tag( `ToolbarSpacer`
                        )->tag( `Button`
                            )->a( n = `text`  v = `Execute`
                            )->a( n = `type`  v = `Emphasized`
                            )->a( n = `press` v = client->_event( `EXECUTE` ) ).

        client->view_display( view->stringify( ) ).

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

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
              )->a( n = `xmlns:core` v = `sap.ui.core`

              )->ele( `Page`
                  )->a( n = `title` v = `Write Output`

              )->tag( n = `HTML` ns = `core`
                  )->a( n = `content` v = client->_bind( html ) ).

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

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `Basic Table`
                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( rows )

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `ID`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Name`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Description`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{ID}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{NAME}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{DESCR}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Table with Sorting

`sap.m.Table` has no built-in column sorting — in abap2UI5, sorting (and filtering) is backend work: react to an event and `SORT` the internal table in ABAP. The new order reaches the rendered view with the response — no re-render, and nothing else to call:

```abap
CLASS z2ui5_cl_app_table_sort DEFINITION PUBLIC.

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

CLASS z2ui5_cl_app_table_sort IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      DO 30 TIMES.
        INSERT VALUE #(
            id     = sy-index
            name   = |Item { sy-index }|
            status = COND #( WHEN sy-index MOD 2 = 0 THEN `open` ELSE `closed` )
          ) INTO TABLE rows.
      ENDDO.

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `Sortable Table`
                  )->ele( `Table`
                      )->a( n = `items` v = client->_bind( rows )
                      )->a( n = `growingThreshold` v = `10`
                      )->a( n = `sticky`           v = `ColumnHeaders`
                      )->a( n = `growing`          b = abap_true

                      )->ele( `headerToolbar`
                          )->ele( `Toolbar`
                              )->tag( `Title`
                                  )->a( n = `text` v = `Orders`
                              )->tag( `ToolbarSpacer`
                              )->tag( `Button`
                                  )->a( n = `text`  v = `Sort by Name`
                                  )->a( n = `press` v = client->_event( `SORT_NAME` )
                              )->tag( `Button`
                                  )->a( n = `text`  v = `Sort by Status`
                                  )->a( n = `press` v = client->_event( `SORT_STATUS` )

                          )->end(
                      )->end(

                      )->ele( `columns`
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `ID`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Name`
                          )->end(
                          )->ele( `Column`
                              )->tag( `Text`
                                  )->a( n = `text` v = `Status`
                          )->end(

                      )->end(

                      )->ele( `items`
                          )->ele( `ColumnListItem`
                              )->ele( `cells`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{ID}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{NAME}`
                                  )->tag( `Text`
                                      )->a( n = `text` v = `{STATUS}` ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `SORT_NAME` ).

      SORT rows BY name.

    ELSEIF client->check_on_event( `SORT_STATUS` ).

      SORT rows BY status.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

For an interactive personalization dialog (column visibility, multi-sort, grouped filters), pair the table with `sap.m.p13n.Engine` or use a `SmartTable` with variant management — supported natively, see [Smart Controls](/cookbook/expert_more/smart_controls).
