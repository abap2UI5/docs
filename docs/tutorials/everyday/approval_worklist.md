---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_030
description: An approval worklist on a RAP business object - the open items, several selected and accepted or rejected with one EXECUTE of the business object's action, and the list shrinking as decisions are made.
---
# Approval Worklist

Somebody has to decide: accept or reject the travel requests, release the
purchase orders, confirm the time sheets. The screen for it is not a
maintenance app. It shows only what is waiting, lets the user decide one or
many at once, and gets shorter with every decision. When it is empty, the
work is done.

The worked example is `Z2UI5_CL_SMPS_APP_030` in
[abap2UI5/samples-stack](https://github.com/abap2UI5/samples-stack) (package
`03`). The travel business object `Z2UI5_R_SMPS_TRV` has two actions,
`acceptTravel` and `rejectTravel`, which set the status.

To install it, pull the branch of that package with abapGit: see
[Getting the Sample](/tutorials/everyday/#getting-the-sample).

## The Shape of the App

| Part | What it does |
|---|---|
| **The list** | The open travels only, oldest first. A `SegmentedButton` switches to the decided ones, so a decision can be looked up |
| **The decision** | Accept and Reject in the table's toolbar, for every selected row at once |
| **The action** | One `EXECUTE` of the business object's action for all selected keys, then `COMMIT ENTITIES` |
| **After** | A fresh read; the decided rows are gone from the list |

## Step 1: Show What Is Waiting

The filter belongs in the `SELECT`. A worklist that reads everything and hides
the decided rows on the screen grows with every decision ever made; one that
selects only the open ones stays as short as the work is:

```abap
DATA(t_status) = VALUE ty_r_status( ( sign   = `I`
                                      option = COND #( WHEN filter = `OPEN` THEN `EQ` ELSE `NE` )
                                      low    = z2ui5_cl_smps_context=>cs_status-open ) ).

SELECT FROM z2ui5_r_smps_trv
  FIELDS TravelId, CustomerId, BeginDate, EndDate, TotalPrice, CurrencyCode, Description, OverallStatus
  WHERE OverallStatus IN @t_status
  ORDER BY BeginDate, TravelId
  INTO TABLE @DATA(t_result) UP TO 100 ROWS.
```

Sort by what makes an item urgent - here the begin date of the trip. The
oldest request at the top is the one the user should look at first.

## Step 2: Offer the Decision Where It Applies

The table runs in `MultiSelect` mode only on the worklist, and the buttons are
visible only there. On the list of decided travels there is nothing to decide:

```abap
DATA(table) = page->ele( `Table`
    )->a( n = `items` v = client->_bind( t_travels )
    )->a( n = `mode`  v = COND #( WHEN filter = `OPEN` THEN `MultiSelect` ELSE `None` ) ).
```

The button types `Accept` and `Reject` give the two buttons their green and
red, which every Fiori user reads without a label.

## Step 3: Call the Action

The action is called like any other operation of the business object: by
key, for many instances at once. The app never writes `OverallStatus` itself;
the behavior definition declares it `readonly`, and the action is the only way
to change it:

```abap
MODIFY ENTITIES OF z2ui5_r_smps_trv
  ENTITY travel
    EXECUTE acceptTravel FROM VALUE #( FOR k IN t_keys ( travelid = k ) )
  RESULT DATA(t_result)
  FAILED DATA(s_failed)
  REPORTED DATA(s_reported).
```

Write two statements, one per action, rather than a dynamic action name. The
action is part of the statement, so the compiler checks that it exists and
that its parameter fits.

The action is declared with `result [1] $self`, so `RESULT` hands back the
changed instances. The confirmation can then say what the business object
did - the new status comes from `%param` - instead of what the app expected
it to do.

Then `COMMIT ENTITIES RESPONSE OF`, and on any `FAILED` a
`ROLLBACK ENTITIES` with the messages of `REPORTED` in a message box. One
decision for twenty travels is one transaction: if the business object
refuses one of them, none of them is decided, and the user sees why.

## Step 4: Read Again

After the save, read the list again. The decided travels leave it because
the `SELECT` no longer finds them, not because the app removed rows from its
table. That way the list shows what the database says, including decisions
someone else made in the meantime.

## Variants

- **A reason for a rejection.** An action can take a parameter. The business
  object declares it with an abstract entity, the app asks for it in a popup
  and passes it as `%param`. `Z2UI5_CL_SMPS_APP_018` in package `12` of
  samples-stack does exactly that for a reject.
- **Only what the user may decide.** With instance feature control, the
  business object says per item whether the action is enabled.
  `GET PERMISSIONS` asks it before the buttons are offered;
  `Z2UI5_CL_SMPS_APP_017` in package `12` shows it.
- **One item at a time.** For decisions that need a closer look, put the
  buttons on a detail page instead, and navigate back to the shortened list
  after each decision - see [Inner App](/cookbook/event_navigation/navigation/inner_app)
  navigation.
- **In the launchpad.** A worklist is a typical tile. The number of open items
  can be the tile's KPI - see [Launchpad](/configuration/launchpad).

## Related

- [Tables](/cookbook/model/tables) - selection and toolbars.
- [EML](/cookbook/eml_cds_sql/eml) - `EXECUTE` and the responses.
- [Message](/cookbook/translation_messages/message) - toasts and message
  boxes.

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Approval Worklist — An approval worklist - the open travels, several selected and accepted or rejected with one EXECUTE, the list shrinking as they are decided | [`Z2UI5_CL_SMPS_APP_030`](https://github.com/abap2UI5/samples-stack/blob/main/src/03/z2ui5_cl_smps_app_030.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `03-rap` |

<!-- samples:end -->
