---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_024
description: Turn an Adobe Form into a PDF from an abap2UI5 app - the generated function module between FP_JOB_OPEN and FP_JOB_CLOSE with the PDF returned instead of printed, one PDF as a download or several as a ZIP, and what ADS needs.
---
# Print Forms as PDF

The invoice, the delivery note, the certificate: most systems already have
the form, built as an Adobe Form years ago and printed from a SAP GUI
transaction. A browser app has no spool dialog and usually no printer - what
the user wants is the PDF, to look at, download, mail or print from the
browser. The form itself stays as it is; only the output changes.

The worked example is `Z2UI5_CL_SMPS_APP_024` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`14`). It renders any form whose name is typed in, and downloads one PDF or a
ZIP of several. It needs Standard ABAP and Adobe Document Services (ADS).

## The Shape of the App

| Step | What happens |
|---|---|
| **Find the function module** | `FP_FUNCTION_MODULE_NAME` returns the name the form generated on this system |
| **Open the job** | `FP_JOB_OPEN` with `getpdf` and `nodialog` - the PDF comes back instead of going to a printer |
| **Render** | The generated function module, with the form's data, returns the PDF in `/1BCDWB/FORMOUTPUT` |
| **Close the job** | `FP_JOB_CLOSE`, in every case |
| **Hand it over** | `download_b64_file`, or a ZIP with `cl_abap_zip` for several forms |

## Step 1: Ask for the Function Module

An Adobe Form generates a function module, and its name differs from system
to system. Never write it down; ask for it:

```abap
TRY.
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = CONV fpname( form )
      IMPORTING
        e_funcname = function.
  CATCH cx_fp_api INTO DATA(api_error).
    error = api_error->get_text( ).
    RETURN.
ENDTRY.
```

## Step 2: Render Into a PDF, Not a Printer

Two output parameters make the difference between printing and returning
the file: `getpdf` hands the PDF back, `nodialog` suppresses the print
dialog, which a browser app has no SAP GUI to show in:

```abap
outputpar-getpdf   = abap_true.
outputpar-nodialog = abap_true.

CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams = outputpar
  EXCEPTIONS
    OTHERS          = 1.
```

Then the generated function module. In a real app you call it with the
parameters of your form's interface - the order, the customer, the items -
plus `/1BCDWB/DOCPARAMS` in and `/1BCDWB/FORMOUTPUT` out. The sample does not
know the form in advance, so it calls it with a parameter table that holds
only those two; a form whose interface wants more says so in the result list.

```abap
CALL FUNCTION function
  EXPORTING
    /1bcdwb/docparams  = docparams
    is_order           = s_order          " your form's own parameters
  IMPORTING
    /1bcdwb/formoutput = output.
```

`output-pdf` is the file.

## Step 3: Close the Job, Always

Call `FP_JOB_CLOSE` whether the rendering worked or not. An open job makes
the next `FP_JOB_OPEN` of the same session fail, and the user sees an error
for a form that would render fine.

## Step 4: Hand the PDF Over

One file goes to the browser as a download:

```abap
client->follow_up_action(
    val   = client->cs_event-download_b64_file
    t_arg = VALUE #( ( |data:application/pdf;base64,{ cl_http_utility=>encode_x_base64( pdf ) }| )
                     ( |{ to_lower( form ) }.pdf| ) ) ).
```

Several go into one ZIP, so the browser asks once instead of once per file:

```abap
DATA(zip) = NEW cl_abap_zip( ).
LOOP AT t_pdf INTO DATA(s_pdf).
  zip->add( name    = |{ to_lower( s_pdf-form ) }.pdf|
            content = s_pdf-pdf ).
ENDLOOP.
DATA(zip_file) = zip->save( ).
```

To show the PDF inside the app rather than download it, put it into a
`PDFViewer` - see [PDF](/cookbook/device_capabilities/pdf). To send it, it is
the attachment of [Send by Mail](/everyday/send_by_mail).

## What the System Needs

- **ADS.** Adobe Document Services do the rendering; the system reaches them
  through the RFC destination `ADS` in `SM59`. When `FP_JOB_OPEN` fails right
  away, this is the first thing to check - the sample says so in its error.
- **ABAP Cloud.** The classic runtime with `FP_JOB_OPEN` is not released
  there. Forms are rendered by the Forms Service by Adobe through
  `cl_fp_ads_util`, with the form template and the data as XML; the shape of
  the app - render in the handler, hand the PDF to the browser - stays the
  same.
- **Many forms.** Rendering takes a noticeable moment per form. A few at a
  time are fine in a roundtrip; for a whole month of invoices, render in a
  background job and offer the result for download when it is done.

## Related

- [PDF](/cookbook/device_capabilities/pdf) - showing a PDF in the view.
- [Upload, Download](/cookbook/device_capabilities/upload_download) - the
  download action and the data URL types it refuses.
- [Send by Mail](/everyday/send_by_mail) - the PDF as an attachment.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Render to PDF, Download One or a ZIP — Renders Adobe Forms of the system to PDF with FP_JOB_OPEN and the generated function module - one form as a PDF download, several at once as one ZIP file | [`Z2UI5_CL_SMPS_APP_024`](https://github.com/abap2UI5/samples-stack/blob/main/src/14/z2ui5_cl_smps_app_024.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `14-adobe-forms` |

<!-- samples:end -->
