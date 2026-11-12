---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_023
description: Send what the app shows by mail - the attachment written in the backend, cl_bcs_mail_message on ABAP Cloud and current releases, the outbound configuration a mail needs, the status after the send, and how to test it.
---
# Send by Mail

"Can you send me that list?" is the most common follow-up to any report. The
job: a button next to the data, a small form for the recipient, the data as
an attachment, and a clear answer whether the mail left the system. Nothing
in it is specific to abap2UI5 except the form - the rest is the mail API of
the system and its configuration, which is where most of the trouble is.

The worked example is `Z2UI5_CL_SMPS_APP_023` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`13`). It sends a table as an `.xlsx` attachment with `cl_bcs_mail_message`.

To install it, pull the branch of that package with abapGit: see
[Getting the Sample](/tutorials/everyday/#getting-the-sample).

## The Shape of the App

| Part | What it does |
|---|---|
| **Form** | Sender, recipient, subject, text - bound attributes, filled in where the app knows the answer |
| **Attachment** | Written in the backend at the moment of sending, from the data the app holds |
| **Send** | `cl_bcs_mail_message`: sender, recipient, subject, body, attachment, `send( )` |
| **Answer** | The status the mail system returns, or the text of the exception |

## Step 1: Fill In What the App Knows

A mail form that starts empty is a form the user abandons. Fill the subject
and the text when the app starts, and the recipient where the record knows
one - the customer's contact, the creator of a request:

```abap
IF client->check_on_init( ).
  subject = `Products as XLSX`.
  text    = |Hello,\n\nthe current product list is attached.\n\nSent from an abap2UI5 app.|.
  view_display( ).
```

The sender is a configuration question, not a user's choice. Most mail
setups accept only addresses of the own domain, or one fixed sender per
system. Fill it from a customizing table or a constant, and show it read
only, rather than letting each user type one the mail server then refuses.

Use `type` `Email` on the address inputs, so a phone shows the keyboard with
the `@`.

## Step 2: Build the Attachment When Sending

Write the file in the handler of the Send button, from the rows the app has,
not from a file the browser kept. The attachment then shows exactly the data
the user saw, and nothing large travels through the roundtrips before it is
needed:

```abap
DATA(xlsx) = z2ui5_cl_smps_xlsx=>write( t_cells ).
```

The same idea works for any format: a PDF from a form (see
[Print Forms as PDF](/tutorials/everyday/print_forms)), a CSV built with
`concat_lines_of( )`, a ZIP of several files with `cl_abap_zip`.

## Step 3: Send

`cl_bcs_mail_message` is the mail API of ABAP Cloud, and available on
on-premise releases from 2023 on:

```abap
TRY.
    DATA(mail) = cl_bcs_mail_message=>create_instance( ).
    mail->set_sender( CONV #( sender ) ).
    mail->add_recipient( CONV #( recipient ) ).
    mail->set_subject( CONV #( subject ) ).
    mail->set_main( cl_bcs_mail_textpart=>create_text_plain( text ) ).
    mail->add_attachment( cl_bcs_mail_binarypart=>create_instance(
                              iv_content      = xlsx
                              iv_content_type = CONV #( z2ui5_cl_smps_xlsx=>mime_type )
                              iv_filename     = `products.xlsx` ) ).
    mail->send( IMPORTING ev_mail_status = DATA(status) ).

  CATCH cx_bcs_mail INTO DATA(error).
    client->message_box_display( text = error->get_text( ) type = `error` ).
ENDTRY.
```

Everything that can go wrong arrives as `cx_bcs_mail`: an address that is
not one, a sender the configuration does not allow, a system without
outbound mail. Show its text. It is the one piece of information the user
can pass on to the person who runs the system.

On older on-premise releases the API is `cl_bcs` with `cl_document_bcs` -
[E-Mail](/cookbook/expert_more/email) shows that variant, attachment
included.

## Step 4: Say What Happened

`send( )` hands the mail to the outbound queue of the system. The status it
returns says whether the mail left already or is waiting there. A mail that
is queued is not an error, but it is not delivered either, so say which one
it is instead of a plain "sent":

```abap
client->message_box_display( text = |Mail handed over to the mail system, status { status }| type = `success` ).
```

## What the System Needs

The code is the small part. A mail only leaves the system when outbound mail
is configured:

| Stack | Configuration | Where to look when nothing arrives |
|---|---|---|
| ABAP Cloud | A communication arrangement for the scenario `SAP_COM_0548` | The communication arrangement and the mail monitoring app of the launchpad |
| On-premise | SMTP in transaction `SCOT` | Transaction `SOST` |

Agree on the sender address with whoever runs the mail server before the
first test. Most failures that look like a bug in the app are a sender the
server does not accept.

## Testing

- Send to yourself first, and open the attachment: a file that downloads
  fine can still be refused by a mail client if its MIME type is wrong.
- On a development system without a real mail server, an SMTP sink such as
  a local test server shows the mail exactly as it would be sent.
- Large attachments are the mail server's limit, not the app's. Several
  megabytes are usually fine; beyond that, send a link to the record
  instead.

## Related

- [E-Mail](/cookbook/expert_more/email) - the classic BCS API.
- [Report and Export](/tutorials/everyday/report_export) - the report a mail button
  usually sits on.
- [Spreadsheet](/cookbook/device_capabilities/spreadsheet) - writing the
  attachment.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| E-Mail — Send a Table as XLSX Attachment — A mail with an .xlsx attachment, sent with cl_bcs_mail_message - recipient, subject and text from the screen, the attachment written with the XCO XLSX library | [`Z2UI5_CL_SMPS_APP_023`](https://github.com/abap2UI5/samples-stack/blob/main/src/13/z2ui5_cl_smps_app_023.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `13-excel-email` |

<!-- samples:end -->
