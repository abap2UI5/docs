CLASS zcl_data_browser DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    " PUBLIC = bound into the view and serialized between roundtrips
    DATA table_name TYPE string.
    DATA rows       TYPE REF TO data.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS view_display.
    METHODS on_event.
    METHODS rows_select.
    METHODS col_label
      IMPORTING
        comp          TYPE abap_componentdescr
      RETURNING
        VALUE(result) TYPE string.
    METHODS model_init.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_data_browser IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      model_init( ).
      view_display( ).
    ELSEIF client->check_on_navigated( ).
      view_display( ).
    ELSEIF client->check_on_event( ).
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD view_display.

    DATA(page) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`        v = `sap.m`
            )->a( n = `xmlns:mvc`    v = `sap.ui.core.mvc`
            )->a( n = `displayBlock` v = `true`
            )->a( n = `height`       v = `100%`

            )->ele( `Page`
                )->a( n = `title` v = `Data Browser` ).

    page->ele( `subHeader`
        )->ele( `Toolbar`
            )->tag( `Input`
                )->a( n = `value`       v = client->_bind( table_name )
                )->a( n = `placeholder` v = `Table name, e.g. T000`
                )->a( n = `width`       v = `18rem`
                )->a( n = `submit`      v = client->_event( `DISPLAY` )

            )->tag( `Button`
                )->a( n = `text`  v = `Display`
                )->a( n = `type`  v = `Emphasized`
                )->a( n = `press` v = client->_event( `DISPLAY` ) ).

    IF rows IS BOUND.

      ASSIGN rows->* TO FIELD-SYMBOL(<rows>).

      " the only question this app asks about the table it was handed
      DATA(comps) = CAST cl_abap_structdescr(
                        CAST cl_abap_tabledescr(
                            cl_abap_typedescr=>describe_by_data( <rows> )
                          )->get_table_line_type( ) )->get_components( ).

      DATA(table) = page->ele( `Table`
                        )->a( n = `items`      v = client->_bind( <rows> )
                        )->a( n = `headerText` v = |{ lines( <rows> ) } rows| ).

      " one column per component - discovered, not declared
      DATA(columns) = table->ele( `columns` ).
      LOOP AT comps INTO DATA(comp).
        columns->ele( `Column`
                  )->ele( `header`
                      )->tag( `Text`
                          )->a( n = `text` v = col_label( comp ) ).
      ENDLOOP.

      " one cell per component, bound by field name
      DATA(cells) = table->ele( `items`
                        )->ele( `ColumnListItem`
                            )->ele( `cells` ).
      LOOP AT comps INTO comp.
        cells->tag( `Text`
                )->a( n = `text` v = |\{{ comp-name }\}| ).
      ENDLOOP.

    ENDIF.

    client->view_display( page->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    CASE client->get_event( ).
      WHEN `DISPLAY`.
        rows_select( ).
        view_display( ).
    ENDCASE.

  ENDMETHOD.

  METHOD rows_select.

    CLEAR rows.
    DATA(name) = CONV tabname( to_upper( table_name ) ).

    " a data browser reads arbitrary tables - this check is not optional
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
      ID 'ACTVT' FIELD '03'
      ID 'TABLE' FIELD name.
    IF sy-subrc <> 0.
      client->message_box_display( text = |Not authorised to display { name }|
                                   type = `error` ).
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA rows TYPE STANDARD TABLE OF (name).
        ASSIGN rows->* TO FIELD-SYMBOL(<rows>).

        SELECT * FROM (name) INTO TABLE @<rows> UP TO 100 ROWS.

      CATCH cx_root.
        CLEAR rows.
        client->message_box_display( text = |{ name } is not a readable table|
                                     type = `error` ).
    ENDTRY.

  ENDMETHOD.

  METHOD col_label.

    result = comp-name.
    IF comp-type->kind <> cl_abap_typedescr=>kind_elem.
      RETURN.
    ENDIF.

    DATA(elem) = CAST cl_abap_elemdescr( comp-type ).
    elem->get_ddic_field( RECEIVING  p_flddescr = DATA(field)
                          EXCEPTIONS not_found  = 1 ).
    IF sy-subrc = 0 AND field-scrtext_m IS NOT INITIAL.
      result = field-scrtext_m.
    ENDIF.

  ENDMETHOD.

  METHOD model_init.
    table_name = `T000`.
  ENDMETHOD.

ENDCLASS.
