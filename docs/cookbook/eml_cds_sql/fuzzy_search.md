---
outline: [2, 4]
description: A typo-tolerant search over an ABAP table with SAP HANA's fuzzy CONTAINS and SCORE, wrapped in a CDS table function and an AMDP so an abap2UI5 app reads it with a plain ABAP SQL SELECT.
---
# Fuzzy Search

SAP HANA can match strings tolerantly — typos, missing letters, transposed characters — with the `CONTAINS` predicate in fuzzy mode: the score is a value between `0.0` (no match) and `1.0` (exact match), and `FUZZY( )` takes the threshold below which a row is filtered out. `CONTAINS` and `SCORE( )` are HANA SQL, not ABAP SQL, so an ABAP program reaches them through an AMDP — here as a CDS table function, which a plain `SELECT` then reads like any view.

Wired to a UI5 `SearchField` in the table toolbar, that gives an ALV-style search that forgives the user's typing.

## What It Needs

- **SAP HANA as the primary database.** The table function is implemented in SQLScript and runs on HANA only; on another database the `SELECT` raises `cx_amdp_execution_failed` at runtime.
- **ABAP 7.40 SP05 or later** for CDS table functions and AMDP; both are available in ABAP Cloud as well.
- The `@Search.searchable` and `@Search.fuzzinessThreshold` annotations of a CDS view are not an alternative here: they drive the `$search` of an OData service (SADL, Fiori elements) and a plain ABAP SQL `SELECT` over such a view ignores them.

## The Table Function

The CDS table function declares the parameters and the result; the AMDP class implements it in SQLScript. The parameter annotated with `#CLIENT` is filled with the session client, so the caller does not pass it:

```sql
@EndUserText.label: 'Customers, fuzzy search'
define table function ZI_CustomerFuzzy
  with parameters
    @Environment.systemField: #CLIENT
    p_client : abap.clnt,
    p_search : abap.char(100)
  returns {
    client : abap.clnt;
    kunnr  : kunnr;
    name1  : name1_gp;
    ort01  : ort01_gp;
    score  : abap.fltp;
  }
  implemented by method zcl_customer_fuzzy=>search;
```

```abap
CLASS zcl_customer_fuzzy DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS search FOR TABLE FUNCTION zi_customerfuzzy.

ENDCLASS.

CLASS zcl_customer_fuzzy IMPLEMENTATION.

  METHOD search BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING kna1.
    RETURN SELECT mandt AS client,
                  kunnr,
                  name1,
                  ort01,
                  score( ) AS score
             FROM kna1
            WHERE mandt = :p_client
              AND contains( ( name1, ort01 ), :p_search, fuzzy( 0.8 ) )
            ORDER BY score DESC;
  ENDMETHOD.

ENDCLASS.
```

`contains( ( name1, ort01 ), … )` searches several columns at once and `score( )` is the best score across them, so the `ORDER BY` ranks the rows by relevance.

## The App

The app is an ordinary abap2UI5 class: the table function is read with ABAP SQL, the rows are bound to a `Table`, and the `SearchField` re-reads them. An empty search string would match nothing in `contains( )`, so the app reads the table directly in that case:

<!-- playground: no Run button — reads a CDS table function over KNA1 with a HANA fuzzy search, and the browser has neither -->
```abap
CLASS zcl_sample_fuzzy DEFINITION PUBLIC.

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

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS load_data.
    METHODS view_display.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_fuzzy IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    me->client = client.

    CASE abap_true.

      WHEN client->check_on_init( ).
        load_data( ).
        view_display( ).

      WHEN client->check_on_navigated( ).
        view_display( ).

      WHEN client->check_on_event( `SEARCH` ).
        load_data( ).

    ENDCASE.

  ENDMETHOD.

  METHOD load_data.

    " ZI_CustomerFuzzy runs CONTAINS( ..., FUZZY( 0.8 ) ) on HANA - see above.
    " Threshold 0.8 tolerates one or two typos in a short word.
    IF mv_search IS INITIAL.
      SELECT FROM kna1
        FIELDS kunnr, name1, ort01
        ORDER BY kunnr
        INTO TABLE @mt_customers
        UP TO 100 ROWS.
    ELSE.
      SELECT FROM zi_customerfuzzy( p_search = @mv_search )
        FIELDS kunnr, name1, ort01
        ORDER BY score DESCENDING
        INTO TABLE @mt_customers
        UP TO 100 ROWS.
    ENDIF.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`     v = `sap.m`
            )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title` v = `Customers`

                    )->ele( `Table`
                        )->a( n = `items`   v = client->_bind( mt_customers )
                        )->a( n = `growing` b = abap_true

                        )->ele( `headerToolbar`
                            )->ele( `OverflowToolbar`
                                )->tag( `Title`
                                    )->a( n = `text` v = `Customer List`
                                )->tag( `ToolbarSpacer`
                                )->tag( `SearchField`
                                    )->a( n = `value`       v = client->_bind( mv_search )
                                    )->a( n = `width`       v = `20rem`
                                    )->a( n = `placeholder` v = `try a misspelled name…`
                                    )->a( n = `search`      v = client->_event( `SEARCH` )

                            )->end(
                        )->end(

                        )->ele( `columns`
                            )->ele( `Column`
                                )->tag( `Text`
                                    )->a( n = `text` v = `Customer`
                            )->end(
                            )->ele( `Column`
                                )->tag( `Text`
                                    )->a( n = `text` v = `Name`
                            )->end(
                            )->ele( `Column`
                                )->tag( `Text`
                                    )->a( n = `text` v = `City`
                            )->end(

                        )->end(

                        )->ele( `items`
                            )->ele( `ColumnListItem`
                                )->ele( `cells`
                                    )->tag( `Text`
                                        )->a( n = `text` v = `{KUNNR}`
                                    )->tag( `Text`
                                        )->a( n = `text` v = `{NAME1}`
                                    )->tag( `Text`
                                        )->a( n = `text` v = `{ORT01}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

Typing `Muller` matches `Müller`, `Hambrug` matches `Hamburg` — the lower the threshold, the more lenient (and the noisier) the result. The `SEARCH` handler only reloads the bound table; the view stays as it is and the rows follow the model.

## Tuning the Threshold

| Threshold | Behavior                                              |
| --------- | ------------------------------------------------------ |
| `1.0`     | Exact match only — same as `LIKE` without wildcards    |
| `0.9`     | Very strict — only tiny variations                     |
| `0.8`     | Default sweet spot — tolerates one or two typos        |
| `0.7`     | Lenient — short words start matching unrelated rows    |
| `< 0.7`   | Mostly noise on real data                              |

The threshold is written into the table function above; to let the app choose it, add a second parameter and pass it to `fuzzy( )`.

::: tip
For the full set of options — `textsearch`, `similarcalculationmode`, `spellcheck` — see the [SAP HANA fuzzy search guide](https://help.sap.com/docs/SAP_HANA_PLATFORM/691cb949c1034198800afde3e5be6570/ee08fb15621c4cb98ce8acaef8b3a48d.html).
:::
