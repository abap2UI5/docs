---
outline: [2, 4]
---
# ABAP SQL

ABAP SQL is the standard way to read and change data in the database directly from ABAP. In an abap2UI5 controller you can issue `SELECT`, `INSERT`, `UPDATE`, `DELETE`, and `MODIFY` statements the same way as in any ABAP program, and bind the result straight to a UI5 view.

### Read Data

The example below selects flights from the `sflight` table and shows them in a UI5 table:
```abap
CLASS z2ui5_cl_sample_sql DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_flights TYPE STANDARD TABLE OF sflight WITH EMPTY KEY.

ENDCLASS.

CLASS z2ui5_cl_sample_sql IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      SELECT FROM sflight
        FIELDS carrid, connid, fldate, price, currency
        ORDER BY carrid, connid, fldate
        INTO TABLE @mt_flights
        UP TO 50 ROWS.

      DATA(view)  = z2ui5_cl_xml_view=>factory( )->shell( )->page( ).
      DATA(table) = view->table( client->_bind( mt_flights ) ).

      table->columns(
           )->column( )->text( `Carrier` )->get_parent(
           )->column( )->text( `Connection` )->get_parent(
           )->column( )->text( `Date` )->get_parent(
           )->column( )->text( `Price` ).

      table->items( )->column_list_item( )->cells(
         )->text( `{CARRID}`
         )->text( `{CONNID}`
         )->text( `{FLDATE}`
         )->text( `{PRICE} {CURRENCY}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

### Filter with a Search Field

Bind the search term with `_bind_edit( )` and re-run the `SELECT` on every `SEARCH` event:
```abap
IF client->check_on_init( ).

  load_data( ).
  render( ).

ELSEIF client->check_on_event( `SEARCH` ).

  load_data( ).
  client->view_model_update( ).

ENDIF.
```

```abap
METHOD load_data.

  SELECT FROM scarr
    FIELDS carrid, carrname, url
    WHERE @mv_search = ''
       OR carrname LIKE @( |%{ mv_search }%| )
    INTO TABLE @mt_carriers
    UP TO 100 ROWS.

ENDMETHOD.
```

### Aggregations and Joins

Aggregations and joins work like in any ABAP report — the result table is then bound to the view:
```abap
SELECT FROM sflight AS f
  INNER JOIN scarr AS c ON c~carrid = f~carrid
  FIELDS f~carrid, c~carrname, SUM( f~seatsocc ) AS total_seats
  GROUP BY f~carrid, c~carrname
  ORDER BY total_seats DESCENDING
  INTO TABLE @DATA(lt_stats)
  UP TO 20 ROWS.
```

### Change Data

`INSERT`, `UPDATE`, `DELETE`, and `MODIFY` are issued from event handlers — wrap them in a transaction and commit explicitly:
```abap
WHEN client->check_on_event( `SAVE` ).

  MODIFY zorders FROM TABLE @mt_orders.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
```

::: tip
For data sourced from the Virtual Data Model, prefer reading from a [CDS view](./cds.md) instead of base tables — you get business semantics, associations, and authorization checks out of the box.
:::
