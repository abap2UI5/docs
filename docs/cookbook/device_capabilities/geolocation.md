---
outline: [2, 4]
---
# Geolocation

abap2UI5 offers a custom control for reading geolocation data from the user's device — longitude, latitude, altitude, speed, and accuracy values. This is handy for logistics apps, field service tools, or any scenario where location matters.

The control fires a `finished` event once the browser resolves the device position, and the binding writes every value back into your ABAP attributes. See also `Z2UI5_CL_SMP_APP_120`.

```abap
CLASS z2ui5_cl_sample_geolocation DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA longitude TYPE string.
    DATA latitude TYPE string.
    DATA altitude TYPE string.
    DATA speed TYPE string.
    DATA altitudeaccuracy TYPE string.
    DATA accuracy TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_geolocation IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`       v = `sap.m`
            )->a( n = `xmlns:mvc`   v = `sap.ui.core.mvc`
            )->a( n = `xmlns:z2ui5` v = `z2ui5.cc`

            )->ele( `Shell`
                )->ele( `Page`
                    )->tag( n = `Geolocation` ns = `z2ui5`
                        )->a( n = `finished`         v = client->_event( `POST` )
                        )->a( n = `longitude`        v = client->_bind( longitude )
                        )->a( n = `latitude`         v = client->_bind( latitude )
                        )->a( n = `altitude`         v = client->_bind( altitude )
                        )->a( n = `altitudeAccuracy` v = client->_bind( altitudeaccuracy )
                        )->a( n = `accuracy`         v = client->_bind( accuracy )
                        )->a( n = `speed`            v = client->_bind( speed ) ).

    client->view_display( view->stringify( ) ).


    CASE client->get( )-event.
      WHEN `POST`.
        "process geolocation data here...
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```

::: tip **Browser Permissions**
Geolocation needs the user to grant permission in the browser. Serve your HTTP endpoint over HTTPS, since most browsers block geolocation on non-secure origins.
:::
