---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_120
description: Read the device position into bound attributes with the invisible Geolocation control - its properties, the finished and error events, and what a denied permission does.
---
# Geolocation

abap2UI5 offers a custom control for reading geolocation data from the user's device — longitude, latitude, altitude, speed, and accuracy values. This is handy for logistics apps, field service tools, or any scenario where location matters.

The control fires a `finished` event once the browser resolves the device position, and the binding writes every value back into your ABAP attributes. It reads the position once per instance, and every view display builds a new one — so display the view in `check_on_navigated( )` only: displayed on every roundtrip, each `finished` would start the next read, without end.

```abap
CLASS zcl_sample_geolocation DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA longitude TYPE string.
    DATA latitude TYPE string.
    DATA altitude TYPE string.
    DATA speed TYPE string.
    DATA altitudeaccuracy TYPE string.
    DATA accuracy TYPE string.
    DATA heading TYPE string.
    DATA error TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_geolocation IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).
      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`       v = `sap.m`
              )->a( n = `xmlns:mvc`   v = `sap.ui.core.mvc`
              )->a( n = `xmlns:z2ui5` v = `z2ui5.cc`

              )->ele( `Shell`
                  )->ele( `Page`
                      )->a( n = `title` v = `Geolocation`

                      )->tag( n = `Geolocation` ns = `z2ui5`
                          )->a( n = `finished`         v = client->_event( `POST` )
                          )->a( n = `error`            v = client->_event( val   = `GEO_ERROR`
                                                                           t_arg = VALUE #( ( `${$parameters>/code}` )
                                                                                            ( `${$parameters>/message}` ) ) )
                          )->a( n = `longitude`        v = client->_bind( longitude )
                          )->a( n = `latitude`         v = client->_bind( latitude )
                          )->a( n = `altitude`         v = client->_bind( altitude )
                          )->a( n = `altitudeAccuracy` v = client->_bind( altitudeaccuracy )
                          )->a( n = `accuracy`         v = client->_bind( accuracy )
                          )->a( n = `speed`            v = client->_bind( speed )
                          )->a( n = `heading`          v = client->_bind( heading )

                      " the control itself draws nothing - it reads the position
                      " and writes it into the bound attributes. Show them.
                      )->ele( `List`
                          )->a( n = `headerText` v = `Device position`
                          )->ele( `items`
                              )->tag( `DisplayListItem`
                                  )->a( n = `label` v = `Latitude`
                                  )->a( n = `value` v = client->_bind( latitude )
                              )->tag( `DisplayListItem`
                                  )->a( n = `label` v = `Longitude`
                                  )->a( n = `value` v = client->_bind( longitude )
                              )->tag( `DisplayListItem`
                                  )->a( n = `label` v = `Altitude`
                                  )->a( n = `value` v = client->_bind( altitude )
                              )->tag( `DisplayListItem`
                                  )->a( n = `label` v = `Accuracy`
                                  )->a( n = `value` v = client->_bind( accuracy )
                              )->tag( `DisplayListItem`
                                  )->a( n = `label` v = `Error`
                                  )->a( n = `value` v = client->_bind( error ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `POST` ).
      "process geolocation data here...

    ELSEIF client->check_on_event( `GEO_ERROR` ).
      error = |{ client->get_event_arg( 1 ) }: { client->get_event_arg( 2 ) }|.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

## Properties and Events

| Property | |
|---|---|
| `longitude`, `latitude`, `altitude`, `accuracy`, `altitudeAccuracy`, `speed`, `heading` | the fields of the browser's position, written as strings; a value the device cannot give — the altitude on a desktop, the heading of a device at rest — arrives as the empty string |
| `enableHighAccuracy` | `true` asks the device for its best source (GPS rather than the network); default `false` |
| `timeout` | milliseconds the browser may take before the read fails with code 3; default `5000`. Empty, `0` or a negative value fall back to the default |

| Event | |
|---|---|
| `finished` | the position was read and the bound properties hold it |
| `error` | the position could not be read; the parameters `code` and `message` say why — `1` permission denied, `2` position unavailable, `3` timeout, and `0` when the browser has no Geolocation API at all, which is what an insecure origin looks like |

The control shows nothing of its own, so without the `error` event a denied permission is invisible: `finished` never fires, the bound attributes stay empty and the console carries one line. Wire the event, as the example does, and tell the user what to allow.

::: tip **Browser Permissions**
Geolocation needs the user to grant permission in the browser. Serve your HTTP endpoint over HTTPS, since most browsers block geolocation on non-secure origins.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Geolocation from the Browser (C) | [`Z2UI5_CL_SMP_APP_120`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_120.clas.abap) |

<!-- samples:end -->
