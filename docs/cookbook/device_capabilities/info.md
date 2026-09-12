---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_122
  - z2ui5_cl_smp_app_445
---
# Info

abap2UI5 ships the current frontend state with every roundtrip. Read it from `client->get( )` — no custom control, no extra event needed. The relevant sub-structures are `s_device`, `s_ui5`, `s_focus`, and `s_scroll`.

## Reading Two of Them

Two of the four are enough to see the mechanism. The class below reads
`s_ui5` and `s_device` in its display branch and shows every field in two
lists. Nothing is requested for it: the browser sent the values along with
the roundtrip that started the app, so `client->get( )` already holds them by
the time `main` runs. Press **Run** to see what your own browser reports:

```abap
CLASS z2ui5_cl_sample_info DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_ui5_version TYPE string.
    DATA mv_ui5_theme   TYPE string.
    DATA mv_ui5_build   TYPE string.
    DATA mv_system      TYPE string.
    DATA mv_browser     TYPE string.
    DATA mv_os          TYPE string.
    DATA mv_viewport    TYPE string.
    DATA mv_touch       TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS z2ui5_cl_sample_info IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(ui5)    = client->get( )-s_ui5.
      DATA(device) = client->get( )-s_device.

      mv_ui5_version = ui5-version.
      mv_ui5_theme   = ui5-theme.
      mv_ui5_build   = ui5-build_timestamp.
      mv_system      = device-system.
      mv_browser     = |{ device-browser-name } { device-browser-version }|.
      mv_os          = |{ device-os-name } { device-os-version }|.
      mv_viewport    = |{ device-resize-width } x { device-resize-height } px|.
      mv_touch       = COND #( WHEN device-support-touch = abap_true THEN `yes` ELSE `no` ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->a( n = `title` v = `Frontend Info`

                  )->ele( `List`
                      )->a( n = `headerText` v = `UI5 — s_ui5`

                      )->ele( `items`

                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `Version`
                              )->a( n = `value` v = client->_bind( mv_ui5_version )
                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `Theme`
                              )->a( n = `value` v = client->_bind( mv_ui5_theme )
                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `Build`
                              )->a( n = `value` v = client->_bind( mv_ui5_build )

                      )->end(

                  )->end(

                  )->ele( `List`
                      )->a( n = `headerText` v = `Device — s_device`

                      )->ele( `items`

                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `System`
                              )->a( n = `value` v = client->_bind( mv_system )
                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `Browser`
                              )->a( n = `value` v = client->_bind( mv_browser )
                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `Operating system`
                              )->a( n = `value` v = client->_bind( mv_os )
                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `Viewport`
                              )->a( n = `value` v = client->_bind( mv_viewport )
                          )->tag( `DisplayListItem`
                              )->a( n = `label` v = `Touch`
                              )->a( n = `value` v = client->_bind( mv_touch ) ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

`s_ui5` names the UI5 runtime the page loaded — its version, its theme and its
build — and `s_device` is what UI5's device API says about the browser, the
operating system, the viewport and touch support. Both are a snapshot of the
roundtrip that built the view; the device model in
[Device Model](/cookbook/model/device_model) is the frontend half of the same
information, which follows a resize without a roundtrip. The four sections
below say where each structure is explained in full.

## Device

For reading device information via `client->get( )-s_device`, see [Device Model](/cookbook/model/device_model).

## UI5

For reading the runtime UI5 framework details via `client->get( )-s_ui5`, see [UI5 Versions](/configuration/ui5_versions).

## Focus

For reading the current focus via `client->get( )-s_focus`, see [Focus](/cookbook/browser_interaction/focus).

## Scroll

For reading scroll positions via `client->get( )-s_scroll`, see [Scrolling](/cookbook/browser_interaction/scrolling).

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://github.com/abap2UI5/samples/blob/main/SAMPLES.md)
that use what this page describes. Each is a single class — pull the repository with
[abapGit](https://abapgit.org) and start it with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Frontend Info: UI5 Version, Theme, OS, Browser | [`Z2UI5_CL_SMP_APP_122`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_122.clas.abap) |
| Device Model: Phone, Tablet, Desktop (A) | [`Z2UI5_CL_SMP_APP_445`](https://github.com/abap2UI5/samples/blob/main/src/01/z2ui5_cl_smp_app_445.clas.abap) |

<!-- samples:end -->
