---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_006
  - z2ui5_cl_smps_app_007
  - z2ui5_cl_smps_app_008
  - z2ui5_cl_smps_app_009
  - z2ui5_cl_smps_app_010
description: Drive a draft-enabled RAP business object from an abap2UI5 app with EML - Edit, the draft update, Activate and Discard, the lock the RAP framework holds, and a minimal app around I_BankTP.
---
# Draft Handling

::: info Two things are called "draft"
This page is about the **RAP draft**: a record a user is still editing. abap2UI5
also saves your app instance between two clicks, in the table `z2ui5_t_01`,
and calls that copy the *app draft*. The two are unrelated. See the
[Glossary](/get_started/glossary).
:::

## Drafts Are a RAP Feature — and You Can Use Them with abap2UI5

RAP draft handling — `with draft` in the behavior definition, a draft table, the standard actions `Edit`, `Resume`, `Activate` and `Discard` — lives inside the business object, so any consumer that speaks EML gets it, and an abap2UI5 app is such a consumer. If you know drafts from RAP, the only new part on this page is wiring the EML to an abap2UI5 view instead of a Fiori Elements UI: skim ahead to [The Four Building Blocks](#the-four-building-blocks).

## What Is a Draft?

A draft is a private, work-in-progress copy of a record. It stays on the server until the user activates or discards it. It is RAP's answer to a stateless web UI, which cannot hold a lock between two clicks the way a Dynpro transaction could.

A helpful mental model:

| Concept | Everyday analogy |
|---|---|
| **Active record** | The published document everyone can read |
| **Draft** | Your private "unsaved changes" copy of that document |
| **Activate** | Hitting *Publish* — the draft replaces the active record |
| **Discard** | Hitting *Close without saving* — the draft is thrown away |

While a draft is open, the underlying record is **locked** so nobody else can edit it at the same time — but the lock is held by SAP's RAP framework, not by your app. That means your abap2UI5 app can stay **stateless**: the user can close the browser, come back tomorrow, and resume exactly where they left off.

In abap2UI5 you drive draft-enabled RAP business objects directly through **EML** (Entity Manipulation Language), exactly like any other entity — see also [EML](/cookbook/eml_cds_sql/eml). There is no abap2UI5-specific draft API and nothing to configure: the EML statements in the examples below would work identically in any ABAP program. What the rest of this page adds is the abap2UI5 part — binding the draft values to input fields, reacting to button events, and walking the user through the draft lifecycle. Time for code.

::: tip You don't have to build anything
On S/4HANA and the BTP ABAP Environment (Steampunk), many business objects already ship as draft-enabled BOs. All examples on this page use **`I_BankTP`**, a draft-enabled BO that ships with S/4HANA. You don't create a BO, and you don't create a draft table — SAP provides both. Your app just calls the standard BO via EML.
:::

## The Draft Lifecycle

Every draft follows the same simple lifecycle. Each arrow in the picture is one EML call, and the rest of the page is the code for each of them:

![The life of a RAP draft: Edit opens a draft from the active record and takes the lock, every change is saved into the draft, Resume continues it later, and Activate writes it to the active record or Discard throws it away](/cookbook/eml_cds_sql/rap-draft-states.svg)

*Edit opens the draft, Resume continues it later, Activate or Discard ends it.*

1. **Edit** — open a draft from the active record (acquires the lock).
2. **Change** — the user types; you save their input into the draft.
3. **Finish** — either **Activate** (write the draft to the database) or **Discard** (throw it away).

A draft waits on the server between two visits. **Resume** continues one that already exists.

## The Four Building Blocks

Everything on this page is built from just four EML operations. If you come from RAP, you will recognize them immediately — they are the standard draft actions every draft-enabled BO ships with. Read these once. The [full edit screen](/cookbook/eml_cds_sql/draft_edit_screen) is simply these four, wired to buttons.

### 1. Open a Draft — `Edit`
Open (acquire) a draft for an existing active record. This takes the lock:
```abap
MODIFY ENTITIES OF i_banktp
       ENTITY Bank
       EXECUTE Edit
       FROM VALUE #( ( %key-BankCountry    = `DE`
                       %key-BankInternalID = `50070010` ) )
       FAILED   DATA(failed)
       REPORTED DATA(reported).

COMMIT ENTITIES.
```

### 2. Read a Draft — `%is_draft = on`
Read the **draft** values (what the user is working on) instead of the active record. The only difference from a normal read is the `%is_draft` flag:
```abap
READ ENTITIES OF i_banktp
  ENTITY Bank
  FIELDS ( SWIFTCode LongBankName )
  WITH VALUE #( ( %key-BankCountry    = `DE`
                  %key-BankInternalID = `50070010`
                  %is_draft           = if_abap_behv=>mk-on ) )  "  on = draft, off = active
  RESULT DATA(drafts).
```

### 3. Activate a Draft — `Activate`
Promote the draft to the active state. **This is the actual save** — the database row is updated:
```abap
MODIFY ENTITIES OF i_banktp
       ENTITY Bank
       EXECUTE Activate
       FROM VALUE #( ( %key-BankCountry    = `DE`
                       %key-BankInternalID = `50070010` ) )
       FAILED   DATA(failed)
       REPORTED DATA(reported).

COMMIT ENTITIES.
```

### 4. Discard a Draft — `Discard`
Throw the draft away and release the lock. The active record is untouched:
```abap
MODIFY ENTITIES OF i_banktp
       ENTITY Bank
       EXECUTE Discard
       FROM VALUE #( ( %key-BankCountry    = `DE`
                       %key-BankInternalID = `50070010` ) )
       FAILED   DATA(failed)
       REPORTED DATA(reported).

COMMIT ENTITIES.
```

::: tip Creating a brand-new record as a draft?
Use `CREATE` with `%is_draft = if_abap_behv=>mk-on` instead of `Edit`. The rest of the lifecycle (read / activate / discard) is identical.

```abap
MODIFY ENTITIES OF i_banktp
       ENTITY Bank
       CREATE FIELDS ( BankCountry BankInternalID LongBankName SWIFTCode )
       WITH VALUE #( ( %cid      = `NEW_BANK`
                       %is_draft = if_abap_behv=>mk-on
                       %data     = VALUE #( BankCountry    = `DE`
                                            BankInternalID = `99999999`
                                            LongBankName   = `My Bank`
                                            SWIFTCode      = `DEUTDEFF` ) ) )
       MAPPED DATA(mapped) FAILED DATA(failed) REPORTED DATA(reported).
COMMIT ENTITIES.
```
:::

## Your First Draft App (Minimal)

Before the full-featured version, here is the **smallest app that actually works**. It does exactly three things: read the record, let the user edit, and save with one button. Start here — once this makes sense, the advanced version is just more buttons.

<!-- playground: no Run button — EML needs a CDS entity and a behavior definition, which only a system has -->
```abap
CLASS zcl_sample_draft_min DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA bank_country     TYPE c LENGTH 3  VALUE `DE`.
    DATA bank_internal_id TYPE c LENGTH 15 VALUE `50070010`.
    DATA swift_code       TYPE c LENGTH 11.
    DATA long_bank_name   TYPE string.
  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS view_display.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sample_draft_min IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_navigated( ).
      " 1. Open a draft so we can edit
      MODIFY ENTITIES OF i_banktp ENTITY Bank EXECUTE Edit
        FROM VALUE #( ( %key-BankCountry    = bank_country
                        %key-BankInternalID = bank_internal_id ) )
        FAILED DATA(f1) REPORTED DATA(r1).
      COMMIT ENTITIES.

      " 2. Read the draft values into our fields
      READ ENTITIES OF i_banktp ENTITY Bank
        FIELDS ( SWIFTCode LongBankName )
        WITH VALUE #( ( %key-BankCountry    = bank_country
                        %key-BankInternalID = bank_internal_id
                        %is_draft           = if_abap_behv=>mk-on ) )
        RESULT DATA(drafts).
      IF drafts IS NOT INITIAL.
        swift_code     = drafts[ 1 ]-SWIFTCode.
        long_bank_name = drafts[ 1 ]-LongBankName.
      ENDIF.
      view_display( ).

    ELSEIF client->check_on_event( `SAVE` ).
      " 3. Push the typed values into the draft, then activate (= save)
      MODIFY ENTITIES OF i_banktp ENTITY Bank
        UPDATE FIELDS ( SWIFTCode LongBankName )
        WITH VALUE #( ( %key-BankCountry      = bank_country
                        %key-BankInternalID   = bank_internal_id
                        %is_draft             = if_abap_behv=>mk-on
                        SWIFTCode             = swift_code
                        LongBankName          = long_bank_name
                        %control-SWIFTCode    = if_abap_behv=>mk-on
                        %control-LongBankName = if_abap_behv=>mk-on ) )
        FAILED DATA(f2) REPORTED DATA(r2).

      MODIFY ENTITIES OF i_banktp ENTITY Bank EXECUTE Activate
        FROM VALUE #( ( %key-BankCountry    = bank_country
                        %key-BankInternalID = bank_internal_id ) )
        FAILED DATA(f3) REPORTED DATA(r3).
      COMMIT ENTITIES.

      client->message_toast_display( `Saved!` ).
    ENDIF.
  ENDMETHOD.

  METHOD view_display.
    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title` v = `Edit Bank (minimal draft)`

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `Bank Name`
                            )->tag( `Input`
                                )->a( n = `value` v = client->_bind( long_bank_name )
                            )->tag( `Label`
                                )->a( n = `text` v = `SWIFT Code`
                            )->tag( `Input`
                                )->a( n = `value` v = client->_bind( swift_code )
                            )->tag( `Button`
                                )->a( n = `text`  v = `Save`
                                )->a( n = `type`  v = `Emphasized`
                                )->a( n = `press` v = client->_event( `SAVE` ) ).

    client->view_display( view->stringify( ) ).
  ENDMETHOD.
ENDCLASS.
```

That's a complete, working draft app. The three numbered comments map one-to-one onto the lifecycle diagram: **open → edit → activate**.

::: warning Why two steps to save?
Notice that *Save* does two things: `UPDATE FIELDS` (copy the user's typed values into the draft) **then** `Activate` (promote the draft to active). The fields are bound with `_bind`, so on each roundtrip the user's input lives in your ABAP variables — but it is **not** in the draft yet until you push it back with `UPDATE FIELDS`. Forgetting this step is the most common beginner mistake: the activate succeeds but saves the *old* values.
:::

## The Full App — A Real-World Edit Screen

The minimal app always opens a draft on startup, which isn't ideal: it locks the record the moment anyone looks at it. A production app starts read-only, switches between a *VIEW* and an *EDIT* mode, handles a draft that already exists, and asks before it throws work away.

[RAP Draft: The Full Edit Screen](/cookbook/eml_cds_sql/draft_edit_screen) builds that app in nine steps and ends with the full class.

## Common Pitfalls

A quick checklist of the mistakes beginners hit most often:

| Symptom | Cause | Fix |
|---|---|---|
| Activate saves the *old* values | Typed values were never pushed into the draft | Call `UPDATE FIELDS` **before** `Activate` ([step 6](/cookbook/eml_cds_sql/draft_edit_screen#_6-saving-typed-values-back-to-the-draft)) |
| Nothing is persisted at all | Missing `COMMIT ENTITIES` | EML stays in the buffer until you commit |
| Read returns the active record, not the draft | Wrong `%is_draft` flag | Use `mk-on` to read the draft, `mk-off` for the active record |
| "Record is locked" errors | A leftover draft from a previous session | `Resume` or `Discard` the existing draft ([step 2](/cookbook/eml_cds_sql/draft_edit_screen#_2-entering-edit-mode-—-check-for-an-existing-draft)) |
| Changes silently lost on exit | No save before leaving edit mode | Save (or prompt to keep) before switching back to VIEW |

For the full story on inspecting `FAILED` / `REPORTED` after EML calls, see the **Failure Handling** section in [EML](/cookbook/eml_cds_sql/eml).

::: tip
The field names (`BankCountry`, `BankInternalID`, `SWIFTCode`, `LongBankName`) and the draft-shadow table (`cabnk_bank_d`) match the released `I_BankTP` on current S/4HANA — on other releases the BO name, fields, or shadow table may differ. If no standard BO covers your object, define your own draft-enabled RAP BO (with its own `draft table z…_d`, `lock master`, etc.) and consume it the same way; see the [SAP RAP draft documentation](https://help.sap.com/docs/abap-cloud/abap-rap/draft). If you need locks for non-draft objects, see [Locks](/cookbook/expert_more/lock).
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/samples-site/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Which Travels Have One — READ ... %is_draft = mk-on | [`Z2UI5_CL_SMPS_APP_006`](https://github.com/abap2UI5/samples-stack/blob/main/src/04/z2ui5_cl_smps_app_006.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `04-rap-draft` |
| Enter Draft Mode — Edit copies the active instance into a new draft, Resume picks up an existing one | [`Z2UI5_CL_SMPS_APP_007`](https://github.com/abap2UI5/samples-stack/blob/main/src/04/z2ui5_cl_smps_app_007.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `04-rap-draft` |
| Change and Save a Draft — UPDATE ... %is_draft = mk-on | [`Z2UI5_CL_SMPS_APP_008`](https://github.com/abap2UI5/samples-stack/blob/main/src/04/z2ui5_cl_smps_app_008.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `04-rap-draft` |
| Leave Draft Mode — EXECUTE Activate / Discard | [`Z2UI5_CL_SMPS_APP_009`](https://github.com/abap2UI5/samples-stack/blob/main/src/04/z2ui5_cl_smps_app_009.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `04-rap-draft` |
| Complete Draft Handling — a whole app, not a snippet - the complete draft lifecycle in one screen | [`Z2UI5_CL_SMPS_APP_010`](https://github.com/abap2UI5/samples-stack/blob/main/src/04/z2ui5_cl_smps_app_010.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `04-rap-draft` |

<!-- samples:end -->
