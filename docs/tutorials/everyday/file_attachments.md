---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_016
description: Store files in a RAP business object and give them back - upload with the FileUploader into a static action with a deep parameter, a list that never loads the content, download by key and delete through the business object.
---
# File Attachments

A contract as PDF, a photo of the damage, the signed delivery note: users
want to keep a file with the record it belongs to, and get it back later. The
job has four parts - upload, list, download, delete - and one decision that
matters more than the others: the business object decides what it stores,
not the app.

The worked example is `Z2UI5_CL_SMPS_APP_016` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`11`). Its business object `Z2UI5_R_SMPS_FIL` keeps one row per file and
takes new files only through its static action `uploadFile`.

To install it, pull the branch of that package with abapGit: see
[Getting the Sample](/tutorials/everyday/#getting-the-sample).

## The Shape of the App

| Part | How |
|---|---|
| **Upload** | `z2ui5:FileUploader` brings the file as a data URL; the app decodes it and calls the action |
| **List** | A `SELECT` on the CDS view with name, type, size and date - never the content |
| **Download** | `READ ENTITIES` by key with the content, back to a data URL, `download_b64_file` |
| **Delete** | `MODIFY ENTITIES ... DELETE`, then `COMMIT ENTITIES` |

## Step 1: Let the Business Object Own the Upload

The behavior definition declares `create` as internal and offers a static
action instead:

```abap
static action uploadFile deep parameter z2ui5_a_smps_file_upload result [1] $self;
```

The action is the only way in, so every file goes through the same checks -
the behavior pool refuses an empty file with a message - no matter whether
it comes from this app, a Fiori elements UI or another program. The
parameter is deep: a description on the first level, and the file one level
below, as content, MIME type and name.

## Step 2: Upload

The FileUploader delivers `data:<mime>;base64,<payload>`. Split it, take the
MIME type from the header - it is the browser's guess from the extension and
may be missing - and decode the payload:

```abap
SPLIT file AT `,` INTO DATA(header) DATA(base64).
DATA(mime_type) = substring_before( val = substring_after( val = header sub = `data:` ) sub = `;` ).
IF mime_type IS INITIAL.
  mime_type = `application/octet-stream`.
ENDIF.
DATA(content) = cl_web_http_utility=>decode_x_base64( base64 ).
```

Then call the action. A static action has no key, so the `%cid` is the
caller's own name for the call:

```abap
MODIFY ENTITIES OF z2ui5_r_smps_fil
  ENTITY file
    EXECUTE uploadfile
    FROM VALUE #( ( %cid   = `UPLOAD_1`
                    %param = VALUE #( description       = description
                                      _streamproperties = VALUE #( filecontent = content
                                                                   mimetype    = mime_type
                                                                   filename    = file_name ) ) ) )
  FAILED DATA(s_failed)
  REPORTED DATA(s_reported).
```

`COMMIT ENTITIES RESPONSE OF` saves it, and a refusal from either statement
goes into a message box. Afterwards clear the attribute that held the data
URL: the app instance is kept between roundtrips, and the file would travel
along with every one of them.

## Step 3: List Without the Content

EML reads by key, so the list is a `SELECT` on the CDS view. Leave the
content column out. The list needs the size of a file, not its bytes, and a
bound table goes to the browser in full:

```abap
SELECT FROM z2ui5_r_smps_fil
  FIELDS FileUuid, FileName, MimeType, FileSize, Description, CreatedAt
  ORDER BY CreatedAt DESCENDING
  INTO TABLE @DATA(t_result) UP TO 50 ROWS.
```

## Step 4: Download by Key

The row's button sends the key with the event,
`` client->_event( val = `DOWNLOAD` arg = `${FILE_UUID}` ) ``, and
`client->get_event_arg( )` reads it. The handler reads that one file with its
content and hands it back the way it came:

```abap
READ ENTITIES OF z2ui5_r_smps_fil
  ENTITY file
    FIELDS ( filename mimetype content )
    WITH VALUE #( ( fileuuid = file_uuid ) )
  RESULT DATA(t_result).

DATA(s_file) = t_result[ 1 ].
client->follow_up_action(
    val   = client->cs_event-download_b64_file
    t_arg = VALUE #( ( |data:{ s_file-mimetype };base64,{ cl_web_http_utility=>encode_x_base64( s_file-content ) }| )
                     ( |{ s_file-filename }| ) ) ).
```

Check `t_result` before the table expression: a file deleted in another tab
is not an exception, the result is simply empty. The frontend refuses data
URLs of an active type such as HTML or SVG, so a stored file cannot run as a
page in the user's browser - see
[Upload, Download](/cookbook/device_capabilities/upload_download#download).

## Step 5: Delete

`MODIFY ENTITIES ... DELETE FROM` by key and a COMMIT. The business object
decides whether a file may be deleted; the app only shows its answer.

## Limits and Variants

- **File size.** The file travels base64-encoded inside one roundtrip and is
  held in one work process, decoded and copied into the business object.
  Request limits of the ICF and the memory of the session end this long
  before the sample does. Files of tens of megabytes belong in a document
  store - SAP Document Management, an object store on BTP - with the
  business object keeping a reference.
- **Attachments of an existing record.** Here the file is its own business
  object. To attach files to, say, a travel, model the file as a child
  entity of the travel and create it by association, or keep the travel's
  key in the file's table.
- **Several files at once.** `UploadSet` takes many files and drag and drop -
  `Z2UI5_CL_SMP_APP_517` in [abap2UI5/samples](https://github.com/abap2UI5/samples)
  shows it.
- **Release.** A deep action parameter with `@Semantics.largeObject` needs
  ABAP Platform 2025 or a current ABAP Cloud release. On older systems, pass
  the file to an action with a flat parameter, or create the instance with
  the content field directly.

## Related

- [Upload, Download](/cookbook/device_capabilities/upload_download) - the
  FileUploader and the download action.
- [EML](/cookbook/eml_cds_sql/eml) - actions, reads and deletes.
- [PDF](/cookbook/device_capabilities/pdf) - showing a stored PDF in the view
  instead of downloading it.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Deep Action Parameter — EXECUTE uploadFile with a deep parameter - the file travels as a stream property, download and delete included | [`Z2UI5_CL_SMPS_APP_016`](https://github.com/abap2UI5/samples-stack/blob/main/src/11/z2ui5_cl_smps_app_016.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `11-rap-file-upload` |

<!-- samples:end -->
