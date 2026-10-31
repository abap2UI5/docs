---
outline: [2, 4]
samples:
  - z2ui5_cl_smp_app_074
  - z2ui5_cl_smp_app_186
  - z2ui5_cl_smp_app_517
description: Upload a file into a bound attribute as a base64 data URL with the FileUploader control, and send one to the browser with the download_b64_file frontend action - properties, limits and the data URL types the download refuses.
---
# Upload, Download

abap2UI5 handles file uploads and downloads by sending base64-encoded data over the binding.

## Upload

The custom control `z2ui5:FileUploader` wraps `sap.ui.unified.FileUploader` with an *Upload* button next to it. The chosen file is read in the browser as a base64 data URL (`data:<mime>;base64,…`) into `value`, its name into `path`, and the `upload` event carries both to the backend:

```abap
CLASS zcl_sample_upload DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mv_path  TYPE string.
    DATA mv_value TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_upload IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`       v = `sap.m`
              )->a( n = `xmlns:mvc`   v = `sap.ui.core.mvc`
              )->a( n = `xmlns:z2ui5` v = `z2ui5.cc`

              )->ele( `Page`
                  )->tag( n = `FileUploader` ns = `z2ui5`
                      )->a( n = `value`       v = client->_bind( mv_value )
                      )->a( n = `path`        v = client->_bind( mv_path )
                      )->a( n = `placeholder` v = `filepath here...`
                      )->a( n = `upload`      v = client->_event( `UPLOAD` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `UPLOAD` ).
      "process mv_value and mv_path here...
      client->message_box_display( `file uploaded` ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

`mv_value` holds the whole file, so a large upload is a large roundtrip and a large draft; split the data URL at the comma and decode the payload with `cl_web_http_utility=>decode_x_base64( )` as [Spreadsheet](/cookbook/device_capabilities/spreadsheet#upload) does.

### Properties and Events

| Property | |
|---|---|
| `value` | the file as a base64 data URL, written by the control |
| `path` | the file name as the inner control reports it |
| `fileType` | the extensions the picker accepts, comma-separated (`xlsx,csv`); empty accepts everything |
| `placeholder` | the text in the empty path field |
| `buttonText` | the text of the browse button; `icon` (default `sap-icon://browse-folder`), `iconOnly` and `buttonOnly` shape it the way they do on `sap.ui.unified.FileUploader` |
| `uploadButtonText` | the text of the *Upload* button, default `Upload` |
| `style` | the button type of the browse button, as `sap.ui.unified.FileUploader` takes it (`Emphasized`, `Transparent`, …) |
| `checkDirectUpload` | `true` sends the file right after it was picked and renders no *Upload* button |
| `multiple` | let the user pick several files; each one arrives in its own `upload` event, in order |
| `enabled`, `visible`, `tooltip` | as on any control |

The control has one event, `upload`. The inner control's `change` is consumed by the control itself — it remembers the picked files and enables the *Upload* button — so there is nothing to wire for it; with `checkDirectUpload` the pick is the upload.

## Download

`download_b64_file` hands the browser a data URL and a file name, and the browser saves it as a download:

```abap
  METHOD z2ui5_if_app~main.

    IF client->check_on_navigated( ).

      DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
          )->ele( n = `View` ns = `mvc`
              )->a( n = `xmlns`     v = `sap.m`
              )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`

              )->ele( `Page`
                  )->tag( `Button`
                      )->a( n = `text`  v = `Download`
                      )->a( n = `press` v = client->_event( `BUTTON_DOWNLOAD` ) ).

      client->view_display( view->stringify( ) ).

    ELSEIF client->check_on_event( `BUTTON_DOWNLOAD` ).

        DATA(lv_name) = `sample.png`.
        DATA(lv_content) = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAApgAAA` &&
        `KYB3X3/OAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAANCSURBVEiJtZZPbBtFFMZ/M7ubXdtdb1xSFyeilBapySVU8h8OoFaooFSqiihIVIp` &&
        `QBKci6KEg9Q6H9kovIHoCIVQJJCKE1ENFjnAgcaSGC6rEnxBwA04Tx43t2FnvDAfjkNibxgHxnWb2e/u992bee7tCa00YFsffekFY+nUzFtjW0LrvjRXrCDIAaPLlW` &&
        `0nHL0SsZtVoaF98mLrx3pdhOqLtYPHChahZcYYO7KvPFxvRl5XPp1sN3adWiD1ZAqD6XYK1b/dvE5IWryTt2udLFedwc1+9kLp+vbbpoDh+6TklxBeAi9TL0taeWpd` &&
        `mZzQDry0AcO+jQ12RyohqqoYoo8RDwJrU+qXkjWtfi8Xxt58BdQuwQs9qC/afLwCw8tnQbqYAPsgxE1S6F3EAIXux2oQFKm0ihMsOF71dHYx+f3NND68ghCu1YIoeP` &&
        `PQN1pGRABkJ6Bus96CutRZMydTl+TvuiRW1m3n0eDl0vRPcEysqdXn+jsQPsrHMquGeXEaY4Yk4wxWcY5V/9scqOMOVUFthatyTy8QyqwZ+kDURKoMWxNKr2EeqVKc` &&
        `TNOajqKoBgOE28U4tdQl5p5bwCw7BWquaZSzAPlwjlithJtp3pTImSqQRrb2Z8PHGigD4RZuNX6JYj6wj7O4TFLbCO/Mn/m8R+h6rYSUb3ekokRY6f/YukArN979jc` &&
        `W+V/S8g0eT/N3VN3kTqWbQ428m9/8k0P/1aIhF36PccEl6EhOcAUCrXKZXXWS3XKd2vc/TRBG9O5ELC17MmWubD2nKhUKZa26Ba2+D3P+4/MNCFwg59oWVeYhkzgN/` &&
        `JDR8deKBoD7Y+ljEjGZ0sosXVTvbc6RHirr2reNy1OXd6pJsQ+gqjk8VWFYmHrwBzW/n+uMPFiRwHB2I7ih8ciHFxIkd/3Omk5tCDV1t+2nNu5sxxpDFNx+huNhVT3` &&
        `/zMDz8usXC3ddaHBj1GHj/As08fwTS7Kt1HBTmyN29vdwAw+/wbwLVOJ3uAD1wi/dUH7Qei66PfyuRj4Ik9is+hglfbkbfR3cnZm7chlUWLdwmprtCohX4HUtlOcQj` &&
        `LYCu+fzGJH2QRKvP3UNz8bWk1qMxjGTOMThZ3kvgLI5AzFfo379UAAAAASUVORK5CYII=`.

        client->follow_up_action(
            val   = client->cs_event-download_b64_file
            t_arg = VALUE #( ( lv_content ) ( lv_name ) ) ).

    ENDIF.

  ENDMETHOD.
```

The first argument is the data URL, `data:<mime>;base64,<payload>`, or a same-origin URL of a file on the system; the second is the file name the browser proposes. The frontend refuses two things and logs the refusal instead of downloading: a URL whose scheme is not `http`, `https`, `data` or `blob`, and a data URL carrying an active type — `text/html`, `application/xhtml`, `text/xml` or `image/svg` — because a data URL of HTML with an attacker-chosen `.html` name is a known drive-by vector. A real download is `application/octet-stream`, an image, a PDF or an Office type. Path separators and control characters in the file name are replaced by `_`, so a name cannot leave the download folder.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Upload to the Backend (C) | [`Z2UI5_CL_SMP_APP_074`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_074.clas.abap) |
| Download to the Browser (A) | [`Z2UI5_CL_SMP_APP_186`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_186.clas.abap) |
| Upload with an UploadSet (C) | [`Z2UI5_CL_SMP_APP_517`](https://github.com/abap2UI5/samples/blob/main/src/z2ui5_cl_smp_app_517.clas.abap) |

<!-- samples:end -->
