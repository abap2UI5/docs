---
outline: [2, 4]
---
# Value Help

A field that asks *"which one?"* is the F4 moment of ABAP. abap2UI5 covers the basics with two built-in popups and lets you build anything custom on top.

#### Suggestions on the Input

The lightest variant — type-ahead from a bound list, no popup, no roundtrip after the initial render. Bind `suggestionitems` to an internal table and pick the columns via the `suggestion_item` template:

```abap
TYPES: BEGIN OF ty_country,
         code TYPE c LENGTH 3,
         name TYPE string,
       END OF ty_country.
DATA mt_countries TYPE STANDARD TABLE OF ty_country.
DATA mv_country   TYPE string.

mt_countries = VALUE #( ( code = `DE` name = `Germany` )
                        ( code = `FR` name = `France`  )
                        ( code = `IT` name = `Italy`   ) ).

client->view_display( z2ui5_cl_xml_view=>factory(
    )->page(
        )->input(
            value             = client->_bind_edit( mv_country )
            showsuggestion    = abap_true
            suggestionitems   = client->_bind( mt_countries )
            )->suggestion_items(
                )->list_item( text = `{CODE}` additionaltext = `{NAME}`
    )->stringify( ) ).
```

#### Selection Popup

For a *"pick from this list"* dialog use the built-in `Z2UI5_CL_POP_TO_SELECT`. Pass any internal table, navigate to it as a sub-app, and read the result on return:

```abap
CLASS z2ui5_cl_sample_f4 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_carrid TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_f4 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        client->view_display( z2ui5_cl_xml_view=>factory(
            )->page(
                )->input(
                    value            = client->_bind_edit( mv_carrid )
                    showvaluehelp    = abap_true
                    valuehelprequest = client->_event( `F4` )
            )->stringify( ) ).

      WHEN client->check_on_event( `F4` ).
        SELECT carrid, carrname, url FROM scarr INTO TABLE @DATA(lt_carriers).
        client->nav_app_call( z2ui5_cl_pop_to_select=>factory(
            i_tab   = lt_carriers
            i_title = `Choose airline` ) ).

      WHEN client->check_on_navigated( ).
        DATA(lo_prev) = CAST z2ui5_cl_pop_to_select( client->get_app_prev( ) ).
        DATA(ls_res)  = lo_prev->result( ).
        IF ls_res-check_confirmed = abap_true.
          FIELD-SYMBOLS <row> TYPE any.
          ASSIGN ls_res-row->* TO <row>.
          ASSIGN COMPONENT `CARRID` OF STRUCTURE <row> TO FIELD-SYMBOL(<carrid>).
          mv_carrid = <carrid>.
        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

Pass `i_multiselect = abap_true` for multi-pick; the result table is then in `ls_res-table`.

#### DDIC Search Help

For value helps that exist as DDIC search help objects (`SE11` → search help), the [generic search help builder](https://github.com/axelmohnen/a2UI5-generic_search_hlp) wraps the F4 framework so you can fire any standard search help by name and get the picked row back. Install it like any other [add-on](../../resources/addons.md).

#### Custom Dialog

When neither popup fits — e.g. a filter bar with multiple columns, ranges, fuzzy search — build the F4 as a separate app with its own view and call it via `nav_app_call`. See [Popup → Separated App](../popups/popup.md#separated-app) for the pattern.
