---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_031
description: The everyday report - a selection turned into range tables, one SELECT on a CDS view entity, a result table with a total per currency, and the rows on the screen exported as an Excel file with XCO.
---
# Report and Export

Most screens in a system are reports: a few fields to narrow the data down, a
table with the result, a total underneath, and a button that hands the same
rows to Excel. In classic ABAP that is a selection screen and an ALV. In
abap2UI5 it is one class with a form, a table and a download, and the
selection logic stays the same.

The worked example is `Z2UI5_CL_SMPS_APP_031` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`13`). It reports on the travels of the business object `Z2UI5_R_SMPS_TRV`
and exports with the XCO XLSX library.

## The Shape of the App

| Part | What it is |
|---|---|
| **Selection** | A `SimpleForm`: agency, customer, a window for the begin date, a row limit. Enter in a field searches |
| **Result** | A table, with the number of rows and the total per currency in its toolbar |
| **Export** | A button that writes the rows on the screen into an `.xlsx` file and downloads it |

The report runs once on start, with an empty selection. A user who opens it
sees data right away and narrows it down from there, instead of facing an
empty table and a form.

## Step 1: The Selection as Range Tables

Bind each field of the form to a component of one structure. When the search
runs, turn each field into a range table: an empty field is an empty range,
and an empty range selects everything. That is the behavior of a selection
screen, without one `IF` per field in the `WHERE`:

```abap
DATA(t_agency) = VALUE ty_r_agency( ).
IF s_selection-agency_id IS NOT INITIAL.
  t_agency = VALUE #( ( sign = `I` option = `EQ` low = s_selection-agency_id ) ).
ENDIF.
```

A date window is a `BT` when both ends are given, a `GE` or `LE` when only one
is. Two `DatePicker` controls with `valueFormat` `yyyyMMdd` deliver the dates
in the internal format, so they go into the range unchanged.

Check what the user typed before it reaches the `SELECT`. A letter in a `NUMC`
field is not an error in ABAP, it silently becomes zeros - and the report
shows nothing without saying why.

## Step 2: One SELECT

Read from the CDS view entity, not from the table. The view carries the
field names the application uses and whatever the data model adds on top -
associations, calculated fields, access control:

```abap
SELECT FROM z2ui5_r_smps_trv
  FIELDS TravelId, AgencyId, CustomerId, BeginDate, EndDate, TotalPrice, CurrencyCode, OverallStatus
  WHERE AgencyId   IN @t_agency
    AND CustomerId IN @t_customer
    AND BeginDate  IN @t_begin
  ORDER BY BeginDate, TravelId
  INTO TABLE @DATA(t_result) UP TO @max_rows ROWS.
```

The row limit is part of the selection, as on a selection screen. When the
result reaches it, say so in the toolbar ("the first 200 - narrow the
selection"), so nobody takes a cut-off list for the whole answer.

## Step 3: Show the Result

The table's row type holds strings formatted for the screen: keys with
`ALPHA = OUT`, dates with `DATE = ISO`. The total is calculated from the
typed result of the `SELECT`, not from those strings, and once per currency -
a sum across currencies is a number without a meaning:

```abap
LOOP AT t_result INTO DATA(result).
  DATA(total) = VALUE ty_s_total( currency = result-currencycode amount = result-totalprice ).
  COLLECT total INTO t_total.
ENDLOOP.
```

## Step 4: Export What the User Sees

The export writes the rows that are on the screen, not the result of a second
`SELECT`. The file then shows exactly what the user looked at, even if
somebody changed the data in between:

```abap
LOOP AT t_travels INTO DATA(travel).
  APPEND VALUE #( a = travel-travel_id
                  b = travel-customer_id
                  ...
                  f = travel-status ) TO t_cells.
ENDLOOP.

DATA(xlsx) = z2ui5_cl_smps_xlsx=>write( t_cells ).

client->follow_up_action(
    val   = client->cs_event-download_b64_file
    t_arg = VALUE #( ( |data:{ z2ui5_cl_smps_xlsx=>mime_type };base64,{ cl_web_http_utility=>encode_x_base64( xlsx ) }| )
                     ( |travels_{ cl_abap_context_info=>get_system_date( ) }.xlsx| ) ) ).
```

The first row carries the column headers. A file name with the date tells
two exports of the same report apart in the user's download folder.

`z2ui5_cl_smps_xlsx` is a small helper of the samples around the XCO XLSX
library; [Spreadsheet](/cookbook/device_capabilities/spreadsheet) shows the
same export with the on-premise APIs and with abap2xlsx.

## Variants

- **Export in the browser.** For a table that is on the screen anyway, the
  browser can write the file itself, without a roundtrip - see
  [Spreadsheet](/cookbook/device_capabilities/spreadsheet#export-in-the-browser).
  The backend export is the right choice when the file needs more than the
  screen shows, or must look the same for everybody.
- **Value help for the fields.** An agency is easier to pick than to
  remember - see [Value Help](/cookbook/expert_more/value_help).
- **Multiple values per field.** A `MultiInput` with tokens maps onto the same
  range table, one row per token, with `EQ` and `BT` as the user entered them.
- **Variants and filter bars.** For many fields, a `FilterBar` with variant
  management saves the user's selections. Package `02` of samples-stack shows
  the classic FilterBar, `Z2UI5_CL_SMPS_APP_493`.
- **Large results.** A report that regularly finds tens of thousands of rows
  wants paging or an aggregation in the `SELECT`, not a longer table - see
  [Size Limit](/cookbook/model/size_limit).

## Related

- [Tables](/cookbook/model/tables) - columns, cells and toolbars.
- [ABAP SQL](/cookbook/eml_cds_sql/abap_sql) and [CDS](/cookbook/eml_cds_sql/cds) -
  reading the data.
- [Upload, Download](/cookbook/device_capabilities/upload_download) - the
  download action.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Excel — Report with Selection and Export — A report as it is written every day - a selection with ranges, the result in a table with a total, and the same rows exported as .xlsx with XCO | [`Z2UI5_CL_SMPS_APP_031`](https://github.com/abap2UI5/samples-stack/blob/main/src/13/z2ui5_cl_smps_app_031.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `13-excel-email` |

<!-- samples:end -->
