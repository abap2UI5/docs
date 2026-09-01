---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_009
  - z2ui5_cl_smp_app_078
---
# Value Help

Value help (the classic ABAP **F4** input help) lets users pick a value from a list instead of typing it. abap2UI5 covers the basics with two built-in popups and lets you build anything custom on top.

### Suggestions on the Input

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

DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
    )->ele( n = `View` ns = `mvc`
        )->a( n = `xmlns`      v = `sap.m`
        )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
        )->a( n = `xmlns:core` v = `sap.ui.core`

        )->ele( `Page`
            )->ele( `Input`
                )->a( n = `value`           v = client->_bind( mv_country )
                )->a( n = `suggestionItems` v = client->_bind( mt_countries )
                )->a( n = `showSuggestion`  b = abap_true

                )->ele( `suggestionItems`
                    )->tag( n = `ListItem` ns = `core`
                        )->a( n = `text`           v = `{CODE}`
                        )->a( n = `additionalText` v = `{NAME}` ).

client->view_display( view->stringify( ) ).
```

### Selection Popup

For a *"pick from this list"* dialog, open a popup with the candidates in it
and let the press event write the chosen value back. No sub-app and no
navigation are involved — the popup is a second view of the same class, so the
selected row is simply an attribute.

```abap
CLASS z2ui5_cl_sample_f4 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_carrier,
        carrid   TYPE c LENGTH 3,
        carrname TYPE c LENGTH 20,
      END OF ty_s_carrier.

    DATA mv_carrid   TYPE string.
    DATA mt_carriers TYPE STANDARD TABLE OF ty_s_carrier WITH EMPTY KEY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_f4 IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_navigated( ).
        DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
                )->a( n = `xmlns`     v = `sap.m`
                )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

                )->ele( `Page`
                    )->tag( `Input`
                        )->a( n = `value`            v = client->_bind( mv_carrid )
                        )->a( n = `valueHelpRequest` v = client->_event( `F4` )
                        )->a( n = `showValueHelp`    b = abap_true ).

        client->view_display( view->stringify( ) ).

      WHEN client->check_on_event( `F4` ).
        " On a system this is your SELECT:
        "   SELECT carrid, carrname FROM scarr INTO TABLE @mt_carriers.
        mt_carriers = VALUE #( ( carrid = `LH` carrname = `Lufthansa` )
                               ( carrid = `UA` carrname = `United Airlines` )
                               ( carrid = `AA` carrname = `American Airlines` )
                               ( carrid = `SQ` carrname = `Singapore Airlines` ) ).

        DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `FragmentDefinition` ns = `core`
                )->a( n = `xmlns`      v = `sap.m`
                )->a( n = `xmlns:core` v = `sap.ui.core`

                )->ele( `Dialog`
                    )->a( n = `title` v = `Choose airline`

                    )->ele( `List`
                        )->a( n = `items` v = client->_bind( mt_carriers )

                        )->ele( `items`
                            )->tag( `StandardListItem`
                                )->a( n = `title`       v = `{CARRNAME}`
                                )->a( n = `description` v = `{CARRID}`
                                )->a( n = `type`        v = `Active`
                                )->a( n = `press`       v = client->_event(
                                                                val = `PICK`
                                                                t_arg = VALUE #( ( `${CARRID}` ) ) ) ) ).

        client->popup_display( popup->stringify( ) ).

      WHEN client->check_on_event( `PICK` ).
        mv_carrid = client->get_event_arg( 1 ).
        client->popup_destroy( ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

The chosen key travels in the event argument, so nothing has to be looked up
again on the way back. `type="Active"` is what makes a `StandardListItem`
pressable — without it the `press` event never fires.

For a ready-made picker with sorting, multi-select and a search field, the
[popups add-on](https://github.com/abap2UI5-addons/popups) carries one.


Pass `i_multiselect = abap_true` for multi-pick; the result table is then in `ls_res-table`.

### DDIC Search Help

For value helps that exist as DDIC search help objects (`SE11` → search help), the [generic search help builder](https://github.com/axelmohnen/a2UI5-generic_search_hlp) wraps the F4 framework so you can fire any standard search help by name and get the picked row back. Install it like any other [add-on](../../advanced/addons.md).

### Custom Dialog

When neither popup fits — e.g. a filter bar with multiple columns, ranges, fuzzy search — build the F4 as a separate app with its own view and call it via `nav_app_call`. See [Popup → Separated App](../popup_popover/popup.md#separated-app) for the pattern.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Value Help: Suggestions and F4 Dialog | [`Z2UI5_CL_SMP_APP_009`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_009.clas.abap) |
| MultiInput with Tokens (C) | [`Z2UI5_CL_SMP_APP_078`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_078.clas.abap) |

<!-- samples:end -->
