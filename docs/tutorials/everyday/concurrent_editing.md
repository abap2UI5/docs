---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_032
description: Two users edit the same RAP instance - optimistic locking in a stateless abap2UI5 app, with LastChangedAt remembered on read and compared after the UPDATE has taken the business object's lock.
---
# Concurrent Editing

Two users open the same travel. The first changes the fee and saves; the
second changes the description a minute later and saves too. Without a check,
the second save writes the fee the second user saw when the popup opened, and
the first change is gone without anyone noticing.

In SAP GUI the answer was an enqueue lock held while the screen was open. An
abap2UI5 app holds no session between two roundtrips, so a lock set when the
popup opens is released before the user types the first character. The
answer here is an optimistic check: remember the version of the data when
reading, and refuse the save if it changed.

The worked example is `Z2UI5_CL_SMPS_APP_032` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`03`), on the travel business object `Z2UI5_R_SMPS_TRV`. Its edit popup has a
*Change by Someone Else* button that plays the second user.

To install it, pull the branch of that package with abapGit: see
[Getting the Sample](/tutorials/everyday/#getting-the-sample).

## The Version of an Instance

A RAP business object usually has it already. The behavior definition of the
travel names its ETag:

```abap
define behavior for z2ui5_r_smps_trv alias Travel
...
etag master LastChangedAt
```

`LastChangedAt` carries the annotation
`@Semantics.systemDateTime.localInstanceLastChangedAt`, so the managed
runtime sets it on every save, whoever saves - this app,
another app, a Fiori elements UI, a background job going through the business
object. A Fiori elements app compares it automatically through the OData
ETag. An app that consumes the business object with EML compares it itself,
in two places.

## Step 1: Remember the Version When Reading

Read the instance fresh when the user opens it, not from the list on the
screen, and keep `LastChangedAt` with the values the user is about to edit:

```abap
READ ENTITIES OF z2ui5_r_smps_trv
  ENTITY travel
    FIELDS ( bookingfee currencycode description lastchangedat )
    WITH VALUE #( ( travelid = travel_id ) )
  RESULT DATA(t_result)
  FAILED DATA(s_failed).

s_edit = VALUE #( ...
                  last_changed_at = t_result[ 1 ]-lastchangedat ).
```

The attribute does not have to be bound to anything. abap2UI5 keeps the app
instance between roundtrips, so it is still there when the user presses Save.

## Step 2: Lock, Then Check, Then Save

The order of the three statements is the whole technique:

```abap
" 1. the UPDATE - the managed runtime takes the lock of the instance here
MODIFY ENTITIES OF z2ui5_r_smps_trv
  ENTITY travel
    UPDATE FIELDS ( bookingfee currencycode description )
    WITH VALUE #( ( travelid = travel_id ... ) )
  FAILED DATA(s_failed)
  REPORTED DATA(s_reported).

" 2. the check - against the database, where this change is not written yet
SELECT SINGLE FROM z2ui5_r_smps_trv
  FIELDS LastChangedAt
  WHERE TravelId = @travel_id
  INTO @DATA(current).

IF current <> s_edit-last_changed_at.
  ROLLBACK ENTITIES.            " drops the change and releases the lock
  " ... tell the user, read the current state
  RETURN.
ENDIF.

" 3. the save
COMMIT ENTITIES RESPONSE OF z2ui5_r_smps_trv
  FAILED DATA(s_failed_commit)
  REPORTED DATA(s_reported_commit).
```

Why this order and not check first, then update:

- **The UPDATE takes the lock.** The travel is its own lock master, so the
  runtime locks the instance when it processes the UPDATE, and holds the lock
  until the COMMIT or the ROLLBACK. If somebody else is saving the same travel
  at this moment, the UPDATE fails right here, with a message in `REPORTED`.
- **The check reads the database.** The UPDATE lives in the transactional
  buffer until the COMMIT, so a `SELECT` still sees the last saved version.
  A `READ ENTITIES` would read the buffer, where the change of this roundtrip
  is already applied.
- **Between the check and the COMMIT, nobody else can save** the travel,
  because the lock is held. A check before the UPDATE would leave a gap in
  which a second save could slip through.

## Step 3: Tell the User

A refused save is not an error of the user. Say what happened and what to do
next, close the popup and show the current data, so the next attempt starts
from what is really saved:

```abap
client->popup_destroy( ).
data_read( ).
view_display( ).
client->message_box_display( text = |Travel { s_edit-travel_id } was changed by someone else after you opened it. | &&
                                    `Your change was not saved - open the travel again to see the current values.`
                             type = `warning` ).
```

Do not offer "save anyway". Overwriting a change the user has not seen is
exactly what the check exists to prevent.

## When Another Strategy Fits Better

| Situation | Strategy |
|---|---|
| Many users, short edits, conflicts are rare | This one: no lock while typing, a check at save |
| The user must see "locked by X" when opening | A soft lock, or a stateful session with an enqueue - both in [Lock](/cookbook/expert_more/lock) |
| Long edits that may be interrupted, a draft-enabled business object | RAP drafts: the draft holds an exclusive lock and survives the session - see [Draft Handling](/cookbook/eml_cds_sql/draft_handling) |
| No business object, a plain table | The same check on a timestamp column of the table - [Lock](/cookbook/expert_more/lock#_3-optimistic-check) |

[Lock](/cookbook/expert_more/lock) compares eight strategies side by side;
this guide is its "combined" strategy for an instance of a RAP business
object, where the business object brings both the lock and the version.

## Related

- [Statefulness](/cookbook/expert_more/statefulness) - why nothing survives a
  roundtrip except the app instance.
- [EML](/cookbook/eml_cds_sql/eml) - `READ ENTITIES`, `MODIFY ENTITIES` and
  the responses.
- [Popup](/cookbook/popup_popover/popup) - the edit dialog.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Edit with Conflict Check — Edit a travel without a stateful session - the app remembers LastChangedAt when it reads, and Save refuses the change if someone else saved the travel in between | [`Z2UI5_CL_SMPS_APP_032`](https://github.com/abap2UI5/samples-stack/blob/main/src/03/z2ui5_cl_smps_app_032.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `03-rap` |

<!-- samples:end -->
