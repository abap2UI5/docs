---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_012
  - z2ui5_cl_smp_app_009
  - z2ui5_cl_smp_app_161
  - z2ui5_cl_smp_app_170
  - z2ui5_cl_smp_app_470
---
# Popup

UI5 offers popups that overlay specific parts of the view. This section walks through building them in abap2UI5.

## General

To show a popup, call `client->popup_display` instead of `client->view_display`:
```abap
  METHOD z2ui5_if_app~main.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `FragmentDefinition` ns = `core`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:core` v = `sap.ui.core`

            )->ele( `Dialog`
                )->a( n = `title` v = `Popup - Info`

                )->tag( `Text`
                    )->a( n = `text` v = `this is information shown in a popup` ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.
```

## Flow Logic
A typical popup flow shows a normal view, opens a popup, and finally closes it. Structure it like this:
```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
        DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `View` ns = `mvc`
                )->a( n = `xmlns`     v = `sap.m`
                )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

                )->ele( `Page`
                    )->a( n = `title` v = `abap2UI5 - Popups`

                    )->tag( `Button`
                        )->a( n = `text`  v = `popup rendering, no background rendering`
                        )->a( n = `press` v = client->_event( `POPUP_OPEN` ) ).

        client->view_display( view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.

      WHEN `POPUP_OPEN`.
        DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `FragmentDefinition` ns = `core`
                )->a( n = `xmlns`      v = `sap.m`
                )->a( n = `xmlns:core` v = `sap.ui.core`

                )->ele( `Dialog`
                    )->a( n = `title` v = `Popup`

                    )->tag( `Text`
                        )->a( n = `text` v = `this is a text in a popup`
                    )->tag( `Button`
                        )->a( n = `text`  v = `close`
                        )->a( n = `press` v = client->_event( `POPUP_CLOSE` ) ).

        client->popup_display( popup->stringify( ) ).


      WHEN `POPUP_CLOSE`.
        client->popup_destroy( ).

    ENDCASE.

  ENDMETHOD.
```

The popup has the same lifecycle as the main view: `popup_display( )` renders the XML and `popup_destroy( )` closes it. Changed ABAP data needs neither — the framework pushes the delta into the already-rendered popup with the response.


## Separated App

For a cleaner source layout, put a popup in its own class and call it via
[navigation](/cookbook/event_navigation/navigation/inner_app). The popup is
then an ordinary `z2ui5_if_app` — it just displays into the popup slot instead
of the main one, and ends by handing control back:

```abap
CLASS z2ui5_cl_sample_confirm DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_question TYPE string.
    DATA mv_confirmed TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_confirm IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    CASE abap_true.

      WHEN client->check_on_navigated( ).
        DATA(popup) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `FragmentDefinition` ns = `core`
                )->a( n = `xmlns`      v = `sap.m`
                )->a( n = `xmlns:core` v = `sap.ui.core`

                )->ele( `Dialog`
                    )->a( n = `title` v = `Please confirm`

                    )->tag( `Text`
                        )->a( n = `text` v = client->_bind( mv_question )

                    )->ele( `buttons`
                        )->tag( `Button`
                            )->a( n = `text`  v = `OK`
                            )->a( n = `press` v = client->_event( `CONFIRM` )
                        )->tag( `Button`
                            )->a( n = `text`  v = `Cancel`
                            )->a( n = `press` v = client->_event( `CANCEL` ) ).

        client->popup_display( popup->stringify( ) ).

      WHEN client->check_on_event( `CONFIRM` ).
        mv_confirmed = abap_true.
        client->nav_app_leave( ).

      WHEN client->check_on_event( `CANCEL` ).
        mv_confirmed = abap_false.
        client->nav_app_leave( ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

The caller sets the question, hands the instance to `nav_app_call( )`, and
reads the answer back off the same instance when control returns:

```abap
CASE abap_true.

  WHEN client->check_on_event( `DELETE` ).
    client->nav_app_call( NEW z2ui5_cl_sample_confirm( mv_question = `Delete this entry?` ) ).

  WHEN client->check_on_navigated( ).
    DATA(lo_prev) = CAST z2ui5_cl_sample_confirm( client->get_app_prev( ) ).
    IF lo_prev IS BOUND AND lo_prev->mv_confirmed = abap_true.
      delete_entry( ).
    ENDIF.
    view_display( ).

ENDCASE.
```

Two things carry the whole pattern: the popup's answer is a **public attribute**
on its own instance, and `get_app_prev( )` is how the caller reaches it. Nothing
is passed back through events.

A ready-made set of these — confirm, select, ranges, file up- and download and
about a dozen more — is the
[popups add-on](https://github.com/abap2UI5-addons/popups), which is versioned
on its own.

To handle multiple stacked popups, note that abap2UI5 shows only one popup at a time on the frontend. But you can keep a popup stack in your backend logic and re-display the previous popup as needed. See `Z2UI5_CL_SMP_APP_161`.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalogue](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Ways to Open a Dialog (A) | [`Z2UI5_CL_SMP_APP_012`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_012.clas.abap) |
| Value Help: Suggestions and F4 Dialog | [`Z2UI5_CL_SMP_APP_009`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_009.clas.abap) |
| Dialog inside a Dialog | [`Z2UI5_CL_SMP_APP_161`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_161.clas.abap) |
| Navigate between Dialogs (NavContainer) (A) | [`Z2UI5_CL_SMP_APP_170`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_170.clas.abap) |
| Element Binding to the Selected Row (A) | [`Z2UI5_CL_SMP_APP_470`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_470.clas.abap) |

<!-- samples:end -->
