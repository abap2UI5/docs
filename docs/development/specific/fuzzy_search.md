---
outline: [2, 4]
---
# Fuzzy Search

On SAP HANA you can match strings tolerantly — typos, missing letters, transposed characters — with the `CONTAINS` SQL function in fuzzy mode. The score is a value between `0.0` (no match) and `1.0` (exact match); pass a threshold to `FUZZY( )` and HANA filters out rows below it.

Wire it to a UI5 `search_field` in the table toolbar and you get an ALV-style search that forgives the user's typing.

#### Minimal Example

```abap
CLASS z2ui5_cl_sample_fuzzy DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    TYPES:
      BEGIN OF ty_customer,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
        ort01 TYPE kna1-ort01,
      END OF ty_customer.
    DATA mt_customers TYPE STANDARD TABLE OF ty_customer WITH EMPTY KEY.
    DATA mv_search    TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_fuzzy IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_init( ).
        load_data( ).
        render( ).

      WHEN client->check_on_event( `SEARCH` ).
        load_data( ).
        client->view_model_update( ).

    ENDCASE.

  ENDMETHOD.

  METHOD load_data.

    "CONTAINS(<col>, <pattern>, FUZZY(<threshold>)) — only available on HANA.
    "Threshold 0.8 ≈ tolerates one or two typos in a short word.
    SELECT FROM kna1
      FIELDS kunnr, name1, ort01
      WHERE @mv_search = ''
         OR contains( ( name1, ort01 ), @mv_search, 'FUZZY(0.8)' ) = 1
      INTO TABLE @mt_customers
      UP TO 100 ROWS.

  ENDMETHOD.

  METHOD render.

    DATA(view) = z2ui5_cl_xml_view=>factory( )->shell( )->page( `Customers` ).
    DATA(tab)  = view->table( client->_bind( mt_customers ) growing = abap_true ).

    tab->header_toolbar( )->overflow_toolbar(
        )->title( `Customers`
        )->toolbar_spacer( )
        )->search_field(
            value       = client->_bind_edit( mv_search )
            width       = `20rem`
            placeholder = `try a misspelled name…`
            search      = client->_event( `SEARCH` ) ).

    tab->columns( )->column( )->text( `Customer` )->get_parent(
                  )->column( )->text( `Name` )->get_parent(
                  )->column( )->text( `City` ).
    tab->items( )->column_list_item( )->cells(
       )->text( `{KUNNR}` )->text( `{NAME1}` )->text( `{ORT01}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

Typing `Muller` matches `Müller`, `Hambrug` matches `Hamburg` — the lower the threshold, the more lenient (and the noisier) the result.

#### Tuning the Threshold

| Threshold | Behaviour                                              |
| --------- | ------------------------------------------------------ |
| `1.0`     | Exact match only — same as `LIKE` without wildcards    |
| `0.9`     | Very strict — only tiny variations                     |
| `0.8`     | Default sweet spot — tolerates one or two typos        |
| `0.7`     | Lenient — short words start matching unrelated rows    |
| `< 0.7`   | Mostly noise on real data                              |

#### Multiple Columns

Pass a list of columns to search several fields at once — HANA returns the best score across them:

```abap
WHERE contains( ( name1, ort01, stras ), @mv_search, 'FUZZY(0.8)' ) = 1
```

#### Returning the Score

To order rows by relevance, expose the fuzzy score with `SCORE( )` and sort on it:

```abap
SELECT FROM kna1
  FIELDS kunnr, name1, ort01,
         score( ) AS rank
  WHERE contains( ( name1, ort01 ), @mv_search, 'FUZZY(0.8)' ) = 1
  ORDER BY rank DESCENDING
  INTO TABLE @mt_customers
  UP TO 100 ROWS.
```

::: warning
`CONTAINS … FUZZY( )` is HANA-only. On non-HANA databases the statement raises an SQL error — guard it behind a release check or fall back to `LIKE` if your code may run elsewhere.
:::

::: tip
For the full set of options — `textsearch`, `similarcalculationmode`, `spellcheck` — see the [SAP HANA fuzzy search guide](https://help.sap.com/docs/SAP_HANA_PLATFORM/691cb949c1034198800afde3e5be6570/ee08fb15621c4cb98ce8acaef8b3a48d.html).
:::
