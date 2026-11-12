---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_029
description: Change many instances of a RAP business object at once - select the rows, describe the change once, preview old and new values, save with one MODIFY ENTITIES ... UPDATE and show each refusal on its row.
---
# Mass Change

Fifty travels need a new booking fee, or all trips of one agency move by a
week. Opening each one is not an option, and a database `UPDATE` would skip
the business object's validations. The job is a screen where the user selects
the rows, says once what should change, sees the result and then saves it all
through the business object.

The worked example is `Z2UI5_CL_SMPS_APP_029` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`03`), on the travel business object `Z2UI5_R_SMPS_TRV`.

To install it, pull the branch of that package with abapGit: see
[Getting the Sample](/tutorials/everyday/#getting-the-sample).

## The Shape of the App

| Roundtrip | What happens |
|---|---|
| **Select** | A table in `MultiSelect` mode; the selection travels back in a bound field of each row |
| **Preview** | The app computes the new values for every selected row and shows them next to the old ones. The business object is not touched |
| **Apply** | One `MODIFY ENTITIES ... UPDATE` for all previewed rows, then `COMMIT ENTITIES`. Every refusal goes back to its row |

The preview is the part users ask for. A change described as "move the begin
date by 30 days" is easy to get wrong, and the table that shows `2026-12-01`
under `2026-11-01` for every row makes the mistake visible before it is saved.

## Step 1: Let the User Select Rows

Give the row structure a flag and bind the table's `selected` property to it.
The binding is two-way, so after the next roundtrip the flag says which rows
the user selected:

```abap
DATA(table) = page->ele( `Table`
    )->a( n = `items` v = client->_bind( t_travels )
    )->a( n = `mode`  v = `MultiSelect` ).

table->ele( `items`
    )->ele( `ColumnListItem`
        )->a( n = `selected` v = `{SELKZ}`
```

The row structure also carries the new values and a status, so the preview
needs no second table: `new_begin_date`, `new_end_date`, `new_fee`, a `state`
for the color and a `status` text.

## Step 2: Describe the Change Once

The form above the table holds the change, not a value per row: a new fee,
and a number of days to move each date by. Convert it **before** looping over
the rows. A fee that is no number is then one message, not fifty:

```abap
TRY.
    IF s_change-booking_fee IS NOT INITIAL.
      fee = s_change-booking_fee.
    ENDIF.
    begin_shift = s_change-begin_shift.
    end_shift = s_change-end_shift.
  CATCH cx_sy_conversion_error.
    client->message_box_display( text = `The fee must be a number, the shifts whole days` type = `error` ).
    RETURN.
ENDTRY.
```

An empty field means "keep the value". That rule belongs in the form's
placeholder text, so the user does not have to guess it.

## Step 3: Preview

For every selected row, compute the new values and write them into the row.
In the table, an `ObjectIdentifier` shows the new value as its title and the
old one below it, which reads as "before and after" without two columns per
field:

```abap
)->tag( `ObjectIdentifier`
    )->a( n = `title` v = `{NEW_BEGIN_DATE}`
    )->a( n = `text`  v = `{BEGIN_DATE}`
```

Keep the preview as the contract: Apply saves what the preview shows, not
what the form says now. The sample remembers that a preview exists and only
enables Apply after one.

## Step 4: Save All Rows at Once

One statement with every row. An UPDATE names its instances by key, so there
is no `%cid` to invent; the key is what every answer carries back:

```abap
MODIFY ENTITIES OF z2ui5_r_smps_trv
  ENTITY travel
    UPDATE FIELDS ( bookingfee begindate enddate )
    WITH VALUE #( FOR u IN t_update
                  ( travelid   = u-travel_id
                    bookingfee = u-booking_fee
                    begindate  = u-begin_date
                    enddate    = u-end_date ) )
  FAILED DATA(s_failed)
  REPORTED DATA(s_reported).
```

`UPDATE FIELDS` lists exactly the fields that change. A field that is not
listed keeps its value even if the structure carries one, which is what makes
a mass change safe: the description of fifty travels is not overwritten with
an empty string.

Then `COMMIT ENTITIES RESPONSE OF`, where the validations run. For both
answers, the refused keys are in `FAILED` and their messages in `REPORTED`
under the same key:

```abap
LOOP AT s_failed_commit-travel INTO DATA(failed_commit).
  DATA(texts) = VALUE string_table( FOR r IN s_reported_commit-travel
                                    WHERE ( travelid = failed_commit-travelid AND %msg IS BOUND )
                                    ( r-%msg->if_message~get_text( ) ) ).
  " ... the texts go into the status of the row with this key
ENDLOOP.
```

In the sample, moving only the begin date by 30 days pushes some trips past
their end date. `validateDates` refuses those rows at the COMMIT, and the
preview table shows the reason on each of them.

## Step 5: After a Refusal

A COMMIT saves all rows or none. After a refusal, call `ROLLBACK ENTITIES` so
the changes leave the transactional buffer, mark the refused rows as errors
and the others as "not saved", and keep the preview on screen. The user can
deselect the refused rows, or change the description of the change, and press
Preview again.

If the rows are independent and the user would rather save what can be saved,
use the retry from [Excel Import into RAP](/tutorials/everyday/excel_import_rap#all-or-nothing-or-row-by-row):
roll back, drop the refused rows, send the rest again.

## Limits and Variants

- **Many rows.** Several hundred rows per COMMIT are fine. For thousands,
  save in packages, and say in the status which package a row was in.
- **Only what the user may change.** If the business object has instance
  feature control, ask it before offering the change: `GET PERMISSIONS` tells
  per row whether an update is allowed, and the preview can mark the rows it
  would not accept. `Z2UI5_CL_SMPS_APP_017` in package `12` of samples-stack
  shows the statement.
- **Draft-enabled business objects.** A mass change usually goes to the
  active instances. If a row has an open draft, the business object refuses
  the update of the active one, and the message says so.

## Related

- [Tables](/cookbook/model/tables) - selection, columns and cells.
- [EML](/cookbook/eml_cds_sql/eml) - `MODIFY ... UPDATE` and the responses.
- [Excel Import into RAP](/tutorials/everyday/excel_import_rap) - the same pattern for
  rows that come from a file.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Mass Change with Preview — Several travels changed in one go - the new values are previewed next to the old ones, one MODIFY ENTITIES ... UPDATE saves them, and a refusal is shown on its row | [`Z2UI5_CL_SMPS_APP_029`](https://github.com/abap2UI5/samples-stack/blob/main/src/03/z2ui5_cl_smps_app_029.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `03-rap` |

<!-- samples:end -->
