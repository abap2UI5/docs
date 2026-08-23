# Somewhere on the Way to UI5, We Lost RTTI

*abap2UI5 Know-How #1 — scheduled for Tuesday, 25 August 2026*

![The dinosaur at a classic ALV grid, the sheep and the sloth at the same table
rendered in UI5, with cl_abap_structdescr->get_components( ) bridging the
two](assets/01-rtti-header.png)

Before Fiori, ABAP developers routinely built screens for tables whose
structure was unknown when the code was written:

```abap
cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                        CHANGING  t_table      = <lt_any> ).
lo_alv->display( ).
```

Two statements, any internal table, no type known at the time of writing.
Before SALV it was `REUSE_ALV_GRID_DISPLAY` and a field catalog assembled by
hand, which is the same idea with more typing. RTTI read the structure at
runtime, the DDIC supplied the labels, and a whole category of tooling was
built on it: data browsers, table maintenance, migration cockpits, half of what
sits in a `Z` package on any system old enough to have one. The generality was the requirement, not a trick.

## Why those tools are still ALV grids

The standard path to a UI5 application goes through a typed OData service. The
shape of the model is decided when the code is written and baked into the
contract — which is exactly what makes a service dependable for clients nobody
controls.

It is also a precondition: the entity type has to exist before anything else
can happen. That is fine when the structure is known, and a dead end when not
knowing it is the entire point of the tool.

So the generic tools stayed where they were. Not because anyone decided ALV was
the right long-term answer, but because the road to anything newer began with a
question they could not answer.

## SE16N, in one class

An abap2UI5 view is a string the application builds, and the model is bound
from ABAP data — including data whose type only exists at runtime. So a small
data browser fits in a single class: type a table name, get its first hundred
rows.

```abap
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
```

The interesting part is what is absent: no entity type, no CDS view, no
service binding, and two loops that name no field. The columns are whatever
`get_components( )` just returned, the binding paths are the component names it
handed back, and `col_label( )` asks the DDIC for the real label — the field
catalog, rebuilt from the same source it always came from.

The `AUTHORITY-CHECK` is not decoration — anything reading an arbitrary table
needs one before it shows a row.

This is the mechanism, not the tool. The full version is the
[se16n addon](https://github.com/abap2UI5-addons/se16n), with filters, paging
and editing, and persistent column layouts are their own problem, solved by
[layout-management](https://github.com/abap2UI5-addons/layout-management).

## What it costs

A generic table has no contract, and that is a real price. Nothing external can
depend on it, nothing announces that the underlying structure changed, and a
locally defined structure gets technical names instead of labels.

So this is not an upgrade over a typed service. One answers what a foreign
system can depend on for the next five years; the other, what an internal tool
should show a user right now, given a structure it was handed a millisecond
ago.

## It sits next to what is already there

None of this asks a system to change direction. abap2UI5 installs with abapGit
and runs behind one ICF node — an app class is one more object, not a
migration, and the services already published stay published.

Which matters for the two setups most systems actually run. Where the business
logic is a RAP business object, an abap2UI5 screen calls it through EML like
any other consumer; the BO never learns that something else is in front of it.
Where the team hand-writes freestyle UI5 against OData, these are the same
`sap.m` controls, and there is no second frontend stack to keep: no service to
define, no BSP to deploy, no separate transport for the UI.

The generic tools are just the natural first thing to move, because they are
the ones a typed service never fitted. Nothing else has to.

## What actually changed

None of the runtime machinery ever went away. `cl_abap_structdescr` is still
there, still released in ABAP Cloud. Only the screen in front of it did.

abap2UI5 gives runtime-typed ABAP a UI5 face again. Not by adding a generic
framework on top, but by binding ABAP data directly, so data typed at runtime
is not a special case. It is just data.

If a tool in your system has been an ALV grid since 2009 because giving it
anything else was never realistic, that is the specific thing that changed.

---

## LinkedIn teaser post

> Before Fiori, screens were routinely built for tables nobody had seen.
> `cl_salv_table=>factory( )` took any internal table and drew it — RTTI
> answered what the columns were, the DDIC supplied the labels, and a whole
> category of ABAP tooling was built that way.
>
> Then the path to a screen started going through a typed service, and the
> entity type had to exist first. So those tools stayed ALV grids — not because
> anyone decided that was right, but because the road to anything newer began
> with a question they could not answer.
>
> A new article on building UI5 views from RTTI at runtime, with a complete
> data browser in one class — and on how it sits next to the RAP and OData
> services a system already runs, rather than in place of them: [link]
>
> Where do you still use RTTI-driven tooling today?
>
> #ABAP #SAP #UI5
