---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_028
description: Import an Excel file into a RAP business object - upload with the FileUploader, read with XCO XLSX, check and preview every row, create all rows with one MODIFY ENTITIES and show each refusal on the row it belongs to.
---
# Excel Import into RAP

A user has a spreadsheet with a few hundred rows and wants them in the system.
The rows belong in a RAP business object, so its validations and
determinations must run for every row exactly as they do for a row typed in by
hand. This guide builds that import as one abap2UI5 app: no OData service, no
action parameter, no frontend project. The business object stays as it is.

The finished app is `Z2UI5_CL_SMPS_APP_028` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`13`). It imports travels into the business object `Z2UI5_R_SMPS_TRV` that
ships with package `03`, and the snippets below are taken from it.

To install it, pull the branch of that package with abapGit: see
[Getting the Sample](/tutorials/everyday/#getting-the-sample).

## The Shape of the App

Three roundtrips, each with one job:

| Roundtrip | What happens | The business object |
|---|---|---|
| **Upload** | The file arrives, XCO reads the first worksheet, every cell is converted into the type of its field | not touched |
| **Preview** | The table shows every row with a status: ready, or what is wrong with it | not touched |
| **Import** | One `MODIFY ENTITIES` creates all ready rows, `COMMIT ENTITIES` saves them, every answer goes back to its row | does all the deciding |

Splitting upload and import is the important decision. A file with a date in
the wrong format should be fixed before anything is created, and a user who
sees the rows first notices a wrong column at a glance. The business object
still decides in the end: the preview only checks what the app can check on
its own.

## Step 1: Bring the File to the Backend

The `z2ui5:FileUploader` custom control reads the file in the browser and
sends it with the next roundtrip as a base64 data URL:

```abap
page->tag( n = `FileUploader` ns = `z2ui5`
    )->a( n = `fileType` v = `xlsx`
    )->a( n = `upload`   v = client->_event( `UPLOAD` )
    )->a( n = `path`     v = client->_bind( file_name )
    )->a( n = `value`    v = client->_bind( file ) ).
```

Everything after the comma is the file. Decode it, and clear the attribute
right away: the app keeps its attributes between roundtrips, and the file
would travel along with every one of them.

```abap
DATA(xlsx) = cl_web_http_utility=>decode_x_base64( substring_after( val = file sub = `,` ) ).
CLEAR file.
```

[Upload, Download](/cookbook/device_capabilities/upload_download) has every
property of the control.

## Step 2: Read the Worksheet

The XCO XLSX library is released for ABAP Cloud and available on-premise from
2023. It maps a range of cells onto an internal table; with the string value
transformation every cell arrives as text, so one row type serves any sheet:

```abap
DATA(worksheet) = xco_cp_xlsx=>document->for_file_content( xlsx
    )->read_access( )->get_workbook( )->worksheet->at_position( 1 ).

DATA(pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
    )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( `A` )
    )->to_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( `F` )
    )->from_row( xco_cp_xlsx=>coordinate->for_numeric_value( 1 )
    )->get_pattern( ).

worksheet->select( pattern )->row_stream(
    )->operation->write_to( REF #( t_cells )
    )->set_value_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value
    )->execute( ).
```

`t_cells` is a table with one string component per column, `a` to `f`. A file
that is no `.xlsx` at all raises an exception here; catch it and show its text
instead of ending the roundtrip with a dump. On a release without XCO, the
[Spreadsheet](/cookbook/device_capabilities/spreadsheet) chapter shows the
same step with `cl_fdt_xl_spreadsheet`.

## Step 3: Convert and Check Every Row

Row 1 is the header. For every row below it the app converts each cell into
the type of its field and collects what does not fit:

- **Numbers with leading zeros** - an ID column formatted as a number arrives
  as `70001`, not `070001`. Check the digits, and let the assignment to the
  `NUMC` field add the zeros.
- **Dates** - a date cell is stored as the number of days since 1899-12-30, a
  cell formatted as text arrives as it was typed. Accept both, and turn the
  first into a date with `CONV d( '18991230' ) + days`. A date such as
  2026-02-30 has the right shape and is still invalid; it converts to the day
  number 0, which is the test for it.
- **Amounts** - assign the text to the amount field inside `TRY` and catch
  `cx_sy_conversion_error`, so `20,00` becomes a message on the row and not a
  dump.

What the app does **not** check is anything the business object decides: here,
that a customer is given and that the end date is not before the begin date.
Checking it twice means two places to keep in step, and the business object's
answer is the one that counts.

The row type of the preview carries the original cells, a `state` and a
`status` text. The state is a `sap.ui.core.ValueState` name, so an
`ObjectStatus` shows it in the right color:

```abap
)->tag( `ObjectStatus`
    )->a( n = `text`  v = `{STATUS}`
    )->a( n = `state` v = `{STATE}`
```

## Step 4: Create All Rows at Once

One `MODIFY ENTITIES` with every ready row, not one statement per row. The
`%cid` is chosen by the caller, and the sample uses the Excel row number for
it:

```abap
MODIFY ENTITIES OF z2ui5_r_smps_trv
  ENTITY travel
    CREATE FIELDS ( agencyid customerid begindate enddate bookingfee currencycode description )
    WITH VALUE #( FOR t IN t_travel
                  ( %cid         = |ROW_{ t-row }|
                    agencyid     = t-agency_id
                    customerid   = t-customer_id
                    begindate    = t-begin_date
                    enddate      = t-end_date
                    bookingfee   = t-booking_fee
                    currencycode = t-currency
                    description  = |{ file_name }, row { t-row }| ) )
  MAPPED DATA(s_mapped)
  FAILED DATA(s_failed)
  REPORTED DATA(s_reported).
```

The `%cid` is the one thing the business object hands back with an instance
before the instance has a key. Built from the row number, it leads every
answer straight back to its row. A refusal of the CREATE itself, such as a
missing authorization, carries the `%cid` in `FAILED` and its messages in
`REPORTED`:

```abap
LOOP AT s_failed-travel INTO DATA(failed).
  row = substring_after( val = failed-%cid sub = `ROW_` ).
  texts = VALUE #( FOR r IN s_reported-travel WHERE ( %cid = failed-%cid AND %msg IS BOUND )
                   ( r-%msg->if_message~get_text( ) ) ).
  " ... the texts go into the status of that row
ENDLOOP.
```

## Step 5: Save, and Map the Answers Back

Validations run at `COMMIT ENTITIES`, not at the CREATE, so a CREATE that
came back clean can still be refused here:

```abap
COMMIT ENTITIES RESPONSE OF z2ui5_r_smps_trv
  FAILED DATA(s_failed_commit)
  REPORTED DATA(s_reported_commit).
```

The answers of the save carry the key, not the `%cid`. `MAPPED` from the
CREATE has both, so it translates one into the other:

```abap
LOOP AT s_failed_commit-travel INTO DATA(failed_commit).
  DATA(mapped) = VALUE #( s_mapped-travel[ travelid = failed_commit-travelid ] OPTIONAL ).
  row = substring_after( val = mapped-%cid sub = `ROW_` ).
  " ... the messages of s_reported_commit with the same key go to that row
ENDLOOP.
```

That works because the travel business object uses early numbering: the key
exists from the CREATE on. With late numbering the final key only exists after
the save; map by the preliminary `%pid` there, which `MAPPED` carries next to
the `%cid`.

After a refused save, call `ROLLBACK ENTITIES`. The instances stay in the
transactional buffer otherwise, and the next EML statement - or the
`COMMIT WORK` abap2UI5 runs at the end of the roundtrip - runs into them.

## All or Nothing, or Row by Row

A COMMIT saves everything or nothing: one refused row, and none of the others
is saved either. Which of the two behaviors is right depends on the data.

- **All or nothing** fits rows that belong together: the lines of one price
  list, the positions of one order. The user fixes the file and uploads it
  again. The sample marks the refused rows as errors and the others as "not
  saved".
- **Row by row** fits rows that stand alone, like the travels here. The sample
  offers it as *Skip refused rows*: after a refused save it rolls back, drops
  the refused rows and runs the same CREATE once more with the rest. One retry
  is enough, because the same rows give the same answers.

Rows that already have a key are left out of the next import, so pressing
Import twice does not create them twice.

## Limits and Variants

- **File size.** The file travels base64-encoded inside the JSON of one
  roundtrip, about a third larger than the file, and sits in the memory of one
  work process. A few thousand rows are no problem; for files the ICF request
  limit refuses, upload in parts or let a background job do the import.
- **Many rows.** For tens of thousands of rows, create in packages of a few
  hundred per COMMIT, so one refused row does not cost the whole file and the
  transactional buffer stays small.
- **Draft instead of active.** If the business object is draft-enabled, create
  the instances with `%is_draft = if_abap_behv=>mk-on` and let the user check
  and activate them in the app that already edits that business object. See
  [Draft Handling](/cookbook/eml_cds_sql/draft_handling).
- **A template for the user.** Offer the empty sheet as a download, written
  with XCO the same way it is read, so the columns are always in the order the
  import expects. The sample's *Download Example File* does that, with one row
  the business object refuses on purpose.
- **A file inside the business object.** When the file itself has to be
  stored, not its rows, the business object takes it as an action parameter -
  package `11` of samples-stack shows that route.

## Related

- [Upload, Download](/cookbook/device_capabilities/upload_download) - the
  FileUploader and the download action.
- [Spreadsheet](/cookbook/device_capabilities/spreadsheet) - XLSX in and out
  without RAP.
- [EML](/cookbook/eml_cds_sql/eml) and [RAP](/cookbook/eml_cds_sql/rap) - the
  statements one by one.
- [Message](/cookbook/translation_messages/message) - message boxes, toasts
  and message lists.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Excel — Import Travels into a RAP Business Object — An .xlsx file of travels checked row by row and created in the RAP business object with one MODIFY ENTITIES - every refusal lands on the row it belongs to | [`Z2UI5_CL_SMPS_APP_028`](https://github.com/abap2UI5/samples-stack/blob/main/src/13/z2ui5_cl_smps_app_028.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `13-excel-email` |

<!-- samples:end -->
