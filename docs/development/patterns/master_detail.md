---
outline: [2, 4]
---
# Master-Detail

The most familiar Fiori floorplan: a list on the left, the picked item's details on the right. Selecting a row updates only the right pane — no full page reload, no app navigation.

The recipe in abap2UI5 is straightforward. Lay out an `hbox` with two children — a list and a detail panel. Both panels read from the same backend data; an `mv_selected` field decides which row the detail panel shows. Selection events update that field, the framework re-renders, the detail panel reflects the new row.

#### Step 1 — The list

Start with the left pane: a basic table bound to your data. Add a `press` event on the row template so a click fires a backend event. Pass the row's key in `t_arg`, otherwise the handler has no way to know which row the user clicked:

```abap
DATA(view) = z2ui5_cl_xml_view=>factory( )->page( `Airlines` ).
DATA(tab) = view->table( client->_bind( mt_carriers ) growing = abap_true ).
tab->columns( )->column( )->text( `Carrier` )->get_parent(
              )->column( )->text( `Name` ).
tab->items( )->column_list_item(
    type  = `Active`
    press = client->_event(
        val   = `SELECT`
        t_arg = VALUE #( ( `${CARRID}` ) ) )
    )->cells( )->text( `{CARRID}` )->text( `{CARRNAME}` ).
```

`type = 'Active'` is what makes a row clickable in `sap.m.Table` — without it `press` fires nothing.

#### Step 2 — Capture the selection

In the event handler, store the picked key. `client->get_event_arg( )` returns the first `t_arg` entry — here the carrier id:

```abap
WHEN client->check_on_event( `SELECT` ).
  mv_selected = client->get_event_arg( ).
  READ TABLE mt_carriers WITH KEY carrid = mv_selected INTO ms_detail.
  client->view_model_update( ).
```

The `view_model_update( )` call refreshes the bound model — the detail panel (built in the next step) picks up the new row automatically.

#### Step 3 — Side-by-side layout

Wrap the list and the detail in an `hbox`. The list gets a fixed width, the detail panel takes the rest. Conditional rendering shows a placeholder when nothing is selected yet:

```abap
DATA(view) = z2ui5_cl_xml_view=>factory( )->page( `Airlines` ).
DATA(hbox) = view->hbox( ).

" left pane — list
DATA(left) = hbox->vbox( width = `30%` ).
" ...build the table from Step 1 inside `left`...

" right pane — detail
DATA(right) = hbox->vbox( width = `70%` ).
IF mv_selected IS INITIAL.
  right->text( `Select an airline on the left.` ).
ELSE.
  right->object_header( title = ms_detail-carrname
                        number = ms_detail-carrid ).
  right->simple_form( editable = abap_false
      )->content( `form`
          )->label( `URL` )->link( text = ms_detail-url href = ms_detail-url ).
ENDIF.
```

#### Full Example

Sales-organisation list on the left, a small detail form on the right — selecting a row updates the detail panel without a full re-render:

```abap
CLASS z2ui5_cl_sample_master_detail DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_carriers TYPE STANDARD TABLE OF scarr WITH EMPTY KEY.
    DATA ms_detail   TYPE scarr.
    DATA mv_selected TYPE scarr-carrid.

ENDCLASS.

CLASS z2ui5_cl_sample_master_detail IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        SELECT FROM scarr FIELDS carrid, carrname, currcode, url
          INTO TABLE @mt_carriers UP TO 50 ROWS.
        render( ).

      WHEN client->check_on_event( `SELECT` ).
        mv_selected = client->get_event_arg( ).
        READ TABLE mt_carriers WITH KEY carrid = mv_selected INTO ms_detail.
        render( ).

    ENDCASE.

  ENDMETHOD.

  METHOD render.

    DATA(view) = z2ui5_cl_xml_view=>factory( )->shell( )->page( `Airlines` ).
    DATA(hbox) = view->hbox( ).

    " ---- left pane (list) ----
    DATA(left) = hbox->vbox( width = `30%` ).
    DATA(tab)  = left->table( client->_bind( mt_carriers ) growing = abap_true ).
    tab->columns( )->column( )->text( `Carrier` )->get_parent(
                  )->column( )->text( `Name` ).
    tab->items( )->column_list_item(
        type  = `Active`
        press = client->_event(
            val   = `SELECT`
            t_arg = VALUE #( ( `${CARRID}` ) ) )
        )->cells( )->text( `{CARRID}` )->text( `{CARRNAME}` ).

    " ---- right pane (detail) ----
    DATA(right) = hbox->vbox( width = `70%` ).
    IF mv_selected IS INITIAL.
      right->message_page(
          text         = `No airline selected`
          description  = `Pick one from the list on the left.`
          icon         = `sap-icon://flight`
          showheader   = abap_false ).
    ELSE.
      right->object_header( title = ms_detail-carrname number = ms_detail-carrid ).
      DATA(form) = right->simple_form( editable = abap_false )->content( `form` ).
      form->label( `Carrier ID` )->text( ms_detail-carrid ).
      form->label( `Currency`   )->text( ms_detail-currcode ).
      form->label( `Website`    )->link( text = ms_detail-url href = ms_detail-url target = `_blank` ).
    ENDIF.

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

::: tip
On a phone the side-by-side layout becomes cramped. Detect the form factor with the [Device Model](../model/device.md) and either stack the panes vertically or navigate to a separate detail app when `device>/system/phone` is true.
:::
