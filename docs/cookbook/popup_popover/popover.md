---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_026
  - z2ui5_cl_smp_app_052
  - z2ui5_cl_smp_app_081
  - z2ui5_cl_smp_app_109
  - z2ui5_cl_smp_app_465
  - z2ui5_cl_smp_app_490
description: Popovers in abap2UI5 - client->popover_display( ) anchors a sap.m.Popover fragment to a control by id, the placement values, and toggleBy for a popover that opens without a roundtrip.
---
# Popover

To show a popover, call `client->popover_display` and pass the ID of the control
the popover should attach to:
```abap
CLASS zcl_sample_popover DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_popover IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title` v = `Popover Example`

                      )->tag( `Button`
                          )->a( n = `id`    v = `TEST`
                          )->a( n = `text`  v = `display popover`
                          )->a( n = `press` v = client->_event( `POPOVER_OPEN` ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

    CASE client->get( )-event.

      WHEN `POPOVER_OPEN`.
        DATA(popover) = z2ui5_cl_ui5_view_builder=>factory(
            )->ele( n = `FragmentDefinition` ns = `core`
                )->a( n = `xmlns`      v = `sap.m`
                )->a( n = `xmlns:core` v = `sap.ui.core`

                )->ele( `Popover`
                    )->a( n = `placement` v = `Left`

                    )->tag( `Text`
                        )->a( n = `text` v = `this is a popover`
                    )->tag( `Button`
                        )->a( n = `id`    v = `my_id`
                        )->a( n = `text`  v = `close`
                        )->a( n = `press` v = client->_event( `POPOVER_CLOSE` ) ).

        client->popover_display(
            xml   = popover->stringify( )
            by_id = `TEST` ).

      WHEN `POPOVER_CLOSE`.
        client->popover_destroy( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

Like popups, a popover picks up changed ABAP values on its own: the framework
compares the model before and after `main( )` and pushes the difference into the
open popover, without re-rendering its XML. A handler that only changes data has
nothing else to call.

`by_id` is the id of the control the popover opens by — `sap.m.Popover`'s
`openBy` — and is usually the button whose press this roundtrip handles.
`popover_destroy( )` closes it; one popover at a time, and a new
`view_display( )` closes it with the screen, as for a popup.

## Placement

`placement` is where the popover lands relative to its anchor, a value of
`sap.m.PlacementType`: `Left`, `Right`, `Top`, `Bottom`, the four
`Preferred…` variants that fall back to the opposite side when there is no
room (`PreferredLeftOrFlip`, `PreferredRightOrFlip`, `PreferredTopOrFlip`,
`PreferredBottomOrFlip`), the pairs that pick one of two sides (`Vertical`,
`Horizontal`, `VerticalPreferredTop`, `VerticalPreferredBottom`,
`HorizontalPreferredLeft`, `HorizontalPreferredRight`) and `Auto`. The
default is `Right`. Sample `Z2UI5_CL_SMP_APP_026` switches between them at
runtime — the value is bound, so changing it is a model push and the next
open uses it.

## Without a Roundtrip: `toggleBy`

A popover that only shows what the view already holds — a menu, a help text,
a `MessagePopover` — does not need the backend to open it. Declare it in the
view, inside the page's `dependents` aggregation so it is part of the control
tree without being rendered in the page, give it an `id`, and toggle it from
the button with `cs_event-control_by_id` and the method `toggleBy`:

```abap
)->ele( `dependents`
    )->ele( `Popover`
        )->a( n = `id`        v = `demoPopover`
        )->a( n = `title`     v = `Details`
        )->a( n = `placement` v = `Bottom`
        )->tag( `Text`
            )->a( n = `text` v = `opened and closed in the browser`
    )->end(
)->end(

)->tag( `Button`
    )->a( n = `text`  v = `Toggle popover`
    )->a( n = `press` v = client->follow_up_action(
                              val   = client->cs_event-control_by_id
                              t_arg = VALUE #( ( `demoPopover` ) ( `toggleBy` ) ( `${$source>/id}` ) ) ) )
```

`toggleBy` opens the popover anchored to the control whose id is the third
argument if it is closed, and closes it if it is open — the controller
pattern `oPopover.openBy( oButton )` / `oPopover.close( )` as one
whitelisted method. The same call works as a statement after a handler, as
`Z2UI5_CL_SMP_APP_465` does; wired into the view as above it is the
no-roundtrip case. The popover's bindings resolve against the app's model
either way, so what it shows is still ABAP data.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Basic Example with Placement | [`Z2UI5_CL_SMP_APP_026`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_026.clas.abap) |
| Open from a Table Row (A) | [`Z2UI5_CL_SMP_APP_052`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_052.clas.abap) |
| Select from a List | [`Z2UI5_CL_SMP_APP_081`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_081.clas.abap) |
| QuickView Contact Card | [`Z2UI5_CL_SMP_APP_109`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_109.clas.abap) |
| Toggle by ID (toggleBy) (A) | [`Z2UI5_CL_SMP_APP_465`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_465.clas.abap) |
| Open Together with the View Build | [`Z2UI5_CL_SMP_APP_490`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_490.clas.abap) |

<!-- samples:end -->
