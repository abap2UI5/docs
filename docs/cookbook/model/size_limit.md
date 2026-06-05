---
outline: [2, 4]
---
# Size Limit

Every UI5 JSON model has a built-in upper limit on the number of items it will expose to a list binding. By default this limit is `100`. If you bind a `ComboBox`, `Table` or any other aggregation to a table that contains more than 100 entries, only the first 100 are rendered — the rest are silently dropped. This is a UI5 design decision, documented under [`sap.ui.model.Model#setSizeLimit`](https://sapui5.hana.ondemand.com/sdk/#/api/sap.ui.model.Model%23methods/setSizeLimit).

abap2UI5 exposes this setting through the built-in client event `SET_SIZE_LIMIT`, so you can raise (or reset) the limit per view directly from ABAP.

### Set the Limit
Trigger the event from your controller with `client->action->gen`. The first argument is the new limit, the second is the view key (`MAIN`, `NEST`, `NEST2`, `POPUP`, `POPOVER`) — use the constants in `client->cs_view` to stay type-safe:
```abap
client->action->gen(
    val   = z2ui5_if_client=>cs_event-set_size_limit
    t_arg = VALUE #(
        ( `1000` )
        ( client->cs_view-main ) ) ).
```
After this call, the model bound to the main view accepts up to 1.000 entries per binding. The setting is remembered across roundtrips — once raised, it stays in effect until you reset it or leave the app.

### Reset the Limit
To restore the default of `100`, omit the limit argument and pass only the view key:
```abap
client->action->gen(
    val   = z2ui5_if_client=>cs_event-set_size_limit
    t_arg = VALUE #( ( client->cs_view-main ) ) ).
```

### Complete Example
The snippet below shows a `ComboBox` filled with 105 entries. Without raising the size limit, the dropdown would stop at item 100. A small form lets the user adjust the limit and the number of entries at runtime:
```abap
CLASS z2ui5_cl_sample_size_limit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_combo,
        key  TYPE string,
        text TYPE string,
      END OF ty_s_combo.
    DATA t_combo         TYPE STANDARD TABLE OF ty_s_combo WITH EMPTY KEY.
    DATA mv_size_limit   TYPE i VALUE 100.
    DATA mv_combo_number TYPE i VALUE 105.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_size_limit IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE client->get( )-event.
      WHEN `UPDATE_LIMIT`.
        client->action->gen(
            val   = z2ui5_if_client=>cs_event-set_size_limit
            t_arg = VALUE #( ( CONV #( mv_size_limit ) ) ( client->cs_view-main ) ) ).
        client->message_toast_display( `Size limit updated` ).
        RETURN.

      WHEN `UPDATE_MODEL`.
        t_combo = VALUE #( ).
        DO mv_combo_number TIMES.
          INSERT VALUE #( key = sy-index text = sy-index ) INTO TABLE t_combo.
        ENDDO.
        client->view_model_update( ).
        RETURN.
    ENDCASE.

    DO mv_combo_number TIMES.
      INSERT VALUE #( key = sy-index text = sy-index ) INTO TABLE t_combo.
    ENDDO.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->page( `Size Limit Demo`
        )->simple_form( title = `Settings` editable = abap_true
            )->content( `form`
                )->label( `setSizeLimit`
                )->input( value = client->_bind_edit( mv_size_limit )
                )->button(
                    text  = `update size limit`
                    press = client->_event( `UPDATE_LIMIT` )
                )->label( `Number of Entries`
                )->input( value = client->_bind_edit( mv_combo_number )
                )->button(
                    text  = `update number of entries`
                    press = client->_event( `UPDATE_MODEL` )
                )->label( `ComboBox`
                )->combobox( items = client->_bind( t_combo )
                    )->item( key = `{KEY}` text = `{TEXT}` ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
```

### Other Views
The same call applies to nested views, popups and popovers — just swap the view key:
```abap
" Popup
client->action->gen(
    val   = z2ui5_if_client=>cs_event-set_size_limit
    t_arg = VALUE #( ( `500` ) ( client->cs_view-popup ) ) ).

" Popover
client->action->gen(
    val   = z2ui5_if_client=>cs_event-set_size_limit
    t_arg = VALUE #( ( `500` ) ( client->cs_view-popover ) ) ).

" Nested view
client->action->gen(
    val   = z2ui5_if_client=>cs_event-set_size_limit
    t_arg = VALUE #( ( `500` ) ( client->cs_view-nested ) ) ).
```

::: tip **When to raise it**
Raise the limit only as high as you actually need. Large bindings increase memory consumption on the frontend and slow down rendering. For very large datasets, prefer a server-side pattern (OData with `growing`, paging, filtering) instead of pushing everything into the model.
:::

For a runnable sample, see `Z2UI5_CL_DEMO_APP_071` in the [samples repository](https://github.com/abap2UI5/samples).
