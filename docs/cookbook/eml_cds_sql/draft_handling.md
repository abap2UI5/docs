---
outline: [2, 4]
---
# Draft Handling

## Drafts Are a RAP Feature — and You Can Use Them with abap2UI5

Draft handling is one of the headline features of the **ABAP RESTful Application Programming Model (RAP)**. If you have built RAP business objects before, you know the drill: add `with draft` to the behavior definition, define a draft table, and the RAP framework takes care of the rest — the shadow table, the pessimistic lock, and the standard draft actions `Edit`, `Resume`, `Activate`, and `Discard`.

What is less well known: **none of this is tied to Fiori Elements or OData.** Because drafts are implemented inside the business object — not inside the UI — *any* consumer that speaks **EML** (Entity Manipulation Language) gets the full draft lifecycle for free. An abap2UI5 app is exactly such a consumer. You call the same draft actions you know from RAP, and SAP's framework handles locking and persistence — your abap2UI5 app just provides the UI on top.

So this page serves two audiences:

- **You know drafts from RAP?** Then the EML on this page is exactly what you already use — the only new part is wiring it to an abap2UI5 view instead of a Fiori Elements UI. Feel free to skim ahead to [The Four Building Blocks](#the-four-building-blocks).
- **You have never heard of drafts?** Read on — the next section explains the concept from scratch. Just keep in mind that drafts are not an abap2UI5 invention: they are a standard part of RAP, and abap2UI5 simply consumes them.

## What Is a Draft?

Imagine a user opens a form, fills in half of it, and then gets pulled into a meeting. With a normal save, they'd have to either commit half-finished (and possibly invalid) data, or lose everything. A **draft** is the third option: a private, work-in-progress copy that is parked safely on the server until the user is ready to finalize it.

Drafts solve a real problem of modern, **stateless** web UIs. Classic Dynpro applications could hold a lock for as long as the user kept the transaction open — a web app cannot, because between two clicks there is no session holding anything. RAP's answer is to persist the intermediate state server-side in a draft table and let the framework manage the lock. In SAP's standard Fiori applications this is what powers the familiar *"keep draft"* behavior — and through EML, your abap2UI5 apps get the very same behavior.

A helpful mental model:

| Concept | Everyday analogy |
|---|---|
| **Active record** | The published document everyone can read |
| **Draft** | Your private "unsaved changes" copy of that document |
| **Activate** | Hitting *Publish* — the draft replaces the active record |
| **Discard** | Hitting *Close without saving* — the draft is thrown away |

While a draft is open, the underlying record is **locked** so nobody else can edit it at the same time — but the lock is held by SAP's RAP framework, not by your app. That means your abap2UI5 app can stay **stateless**: the user can close the browser, come back tomorrow, and resume exactly where they left off.

In abap2UI5 you drive draft-enabled RAP business objects directly through **EML** (Entity Manipulation Language), exactly like any other entity — see also [EML](./eml.md). There is no abap2UI5-specific draft API and nothing to configure: the EML statements in the examples below would work identically in any ABAP program. What the rest of this page adds is the abap2UI5 part — binding the draft values to input fields, reacting to button events, and walking the user through the draft lifecycle. Time for code.

::: tip You don't have to build anything
On S/4HANA and the BTP ABAP Environment (Steampunk), many business objects already ship as draft-enabled BOs. All examples on this page use **`I_BankTP`**, a draft-enabled BO that ships with S/4HANA. You don't create a BO, and you don't create a draft table — SAP provides both. Your app just calls the standard BO via EML.
:::

## The Draft Lifecycle

Every draft follows the same simple lifecycle. Get this picture in your head and the rest of the page is just code for each arrow:

```
                            Activate
                         ┌────────────▶  ACTIVE record updated  ✔
                         │              (draft saved to database)
   Edit      ┌───────────┴──┐
ACTIVE ─────▶│    DRAFT      │
record       │  (user edits) │
             └───────────┬──┘
                         │
                         └────────────▶  Draft thrown away  ✘
                            Discard       (active record unchanged)
```

1. **Edit** — open a draft from the active record (acquires the lock).
2. **Change** — the user types; you save their input into the draft.
3. **Finish** — either **Activate** (write the draft to the database) or **Discard** (throw it away).

## The Four Building Blocks

Everything on this page is built from just four EML operations. If you come from RAP, you will recognize them immediately — they are the standard draft actions every draft-enabled BO ships with. Read these once — the full app below is simply these four, wired to buttons.

#### 1. Open a Draft — `Edit`
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

#### 2. Read a Draft — `%is_draft = on`
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

#### 3. Activate a Draft — `Activate`
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

#### 4. Discard a Draft — `Discard`
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

```abap
CLASS z2ui5_cl_sample_draft_min DEFINITION PUBLIC.
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

CLASS z2ui5_cl_sample_draft_min IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ).
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
    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page( `Edit Bank (minimal draft)`
            )->simple_form( editable = abap_true
                )->content( `form`
                )->label( `Bank Name`
                )->input( client->_bind( long_bank_name )
                )->label( `SWIFT Code`
                )->input( client->_bind( swift_code )
                )->button(
                    text  = `Save`
                    type  = `Emphasized`
                    press = client->_event( `SAVE` ) ).
    client->view_display( view->stringify( ) ).
  ENDMETHOD.
ENDCLASS.
```

That's a complete, working draft app. The three numbered comments map one-to-one onto the lifecycle diagram: **open → edit → activate**.

::: warning Why two steps to save?
Notice that *Save* does two things: `UPDATE FIELDS` (copy the user's typed values into the draft) **then** `Activate` (promote the draft to active). The fields are two-way bound with `_bind_edit`, so on each roundtrip the user's input lives in your ABAP variables — but it is **not** in the draft yet until you push it back with `UPDATE FIELDS`. Forgetting this step is the most common beginner mistake: the activate succeeds but saves the *old* values.
:::

## The Full App — A Real-World Edit Screen

The minimal app always opens a draft on startup, which isn't ideal: it locks the record the moment anyone looks at it. A production app needs a few more behaviors:

- **Start read-only.** Show the active record first; only lock when the user clicks *Edit*.
- **Two modes.** *VIEW* (read-only, no draft) and *EDIT* (draft open, inputs editable).
- **Handle an existing draft.** What if the user (or someone else) already has a draft open for this record?
- **Confirm before discarding.** Don't silently throw away work the user typed.

The rest of this page builds that app step by step. It uses two modes:

- **VIEW mode** — read-only display of the active database record. No lock, no draft.
- **EDIT mode** — a draft is open; inputs are editable. The RAP framework holds the lock.

Toggling between the modes drives the entire draft lifecycle (acquire, resume, save, activate, discard).

#### 1. App Startup — Always Begin in View Mode
On `on_init` the app reads the **active** record and renders it read-only. No draft is touched yet, so other users can still edit the same record.
```abap
METHOD on_init.
  mode       = `VIEW`.
  draft_open = abap_false.
  read_active( ).
  status_text = `View mode — click Edit to make changes.`.
  view_display( ).
ENDMETHOD.

METHOD read_active.
  READ ENTITIES OF i_banktp
    ENTITY Bank
    FIELDS ( SWIFTCode LongBankName )
    WITH VALUE #( ( %key-BankCountry    = bank_country
                    %key-BankInternalID = bank_internal_id
                    %is_draft           = if_abap_behv=>mk-off ) )
    RESULT DATA(actives).
  IF actives IS NOT INITIAL.
    swift_code     = actives[ 1 ]-SWIFTCode.
    long_bank_name = actives[ 1 ]-LongBankName.
  ENDIF.
ENDMETHOD.
```
`%is_draft = if_abap_behv=>mk-off` makes the read return the **active** row, not any open draft.

#### 2. Entering Edit Mode — Check for an Existing Draft
When the user clicks **Switch to Edit Mode**, the app first looks in the BO's draft-shadow table (here `cabnk_bank_d`) joined with `sdraft_admin` to find out whether a draft already exists for this key — and who owns it. (Simplified here; the full snippet at the end of the page additionally reads the draft's timestamp.)
```abap
METHOD check_existing_draft.
  SELECT SINGLE a~created_by, a~last_changed_at
    FROM cabnk_bank_d AS d
    INNER JOIN sdraft_admin AS a
      ON a~draft_key = d~draftadministrativedatauuid
    WHERE d~bankcountry    = @bank_country
      AND d~bankinternalid = @bank_internal_id
    INTO @DATA(draft_info).

  IF sy-subrc <> 0.
    " No existing draft — create a fresh one and switch to EDIT
    draft_acquire( ).
    draft_read( ).
    mode = `EDIT`.
    view_display( ).
    RETURN.
  ENDIF.

  IF draft_info-created_by = sy-uname.
    " Same user — popup with Resume / Discard choice
    client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory(
        i_title               = `Existing Draft Found`
        i_question_text       = `You have an open draft. Resume working on it, or discard and start fresh?`
        i_icon                = `sap-icon://edit`
        i_button_text_confirm = `Resume Draft`
        i_button_text_cancel  = `Discard && Start New`
        i_event_confirm       = `STARTUP_RESUME`
        i_event_cancel        = `STARTUP_DISCARD` ) ).
  ELSE.
    " Different user holds the draft — stay in VIEW mode
    status_text = |Draft locked by user { draft_info-created_by }. Cannot enter edit mode.|.
    view_display( ).
  ENDIF.
ENDMETHOD.
```
Three branches:
- **no draft** — call `draft_acquire( )` and go straight to EDIT,
- **same user** — show a popup so the user can resume or throw the draft away,
- **other user** — refuse and stay in VIEW with an explanatory status text.

::: tip
The shadow-table name (`cabnk_bank_d`) is BO-specific. To find it for your own BO, check the `draft table` keyword in its behavior definition.
:::

#### 3. Acquiring a Fresh Draft
A new draft is created by the `Edit` action. `%param-preserve_changes = abap_true` tells RAP to keep any existing draft instead of overwriting it — defensive even though we already checked.
```abap
METHOD draft_acquire.
  CLEAR messages.
  MODIFY ENTITIES OF i_banktp
    ENTITY Bank
    EXECUTE Edit
      FROM VALUE #( ( %cid                    = 'EDIT_BANK'
                      %key-BankCountry        = bank_country
                      %key-BankInternalID     = bank_internal_id
                      %param-preserve_changes = abap_true ) )
    FAILED   DATA(failed)
    REPORTED DATA(reported).
  " ...collect messages from reported-bank / reported-%other...
  IF failed IS NOT INITIAL.
    draft_open  = abap_false.
    status_text = `Could not open draft — see messages below.`.
    RETURN.
  ENDIF.
  COMMIT ENTITIES.
  draft_open = abap_true.
ENDMETHOD.
```
After a successful `Edit`, the lock is held by the RAP framework and a row exists in the draft-shadow table. `draft_open = abap_true` switches the view's inputs from read-only to editable.

#### 4. Resuming an Existing Draft
If the user picks **Resume Draft**, the `Resume` action re-takes the lock on the existing draft without overwriting its contents.
```abap
METHOD draft_resume.
  MODIFY ENTITIES OF i_banktp
    ENTITY Bank
    EXECUTE Resume
      FROM VALUE #( ( %key-BankCountry    = bank_country
                      %key-BankInternalID = bank_internal_id ) )
    FAILED   DATA(failed)
    REPORTED DATA(reported).
  IF failed IS INITIAL.
    COMMIT ENTITIES.
    draft_open = abap_true.
  ENDIF.
ENDMETHOD.
```

#### 5. Reading the Draft into the App's Fields
Once a draft is open, read it with `%is_draft = if_abap_behv=>mk-on` so the inputs bind to draft values rather than the active record.
```abap
METHOD draft_read.
  IF draft_open = abap_false.
    RETURN.
  ENDIF.
  READ ENTITIES OF i_banktp
    ENTITY Bank
    FIELDS ( SWIFTCode LongBankName )
    WITH VALUE #( ( %key-BankCountry    = bank_country
                    %key-BankInternalID = bank_internal_id
                    %is_draft           = if_abap_behv=>mk-on ) )
    RESULT DATA(drafts).
  IF drafts IS NOT INITIAL.
    swift_code     = drafts[ 1 ]-SWIFTCode.
    long_bank_name = drafts[ 1 ]-LongBankName.
  ENDIF.
ENDMETHOD.
```

#### 6. Saving Typed Values Back to the Draft
The fields are two-way bound (`client->_bind( … )`), so user input lives in ABAP variables on the next roundtrip. To persist them in the draft, push them back with `UPDATE FIELDS`.
```abap
METHOD save_current_to_draft.
  MODIFY ENTITIES OF i_banktp
    ENTITY Bank
    UPDATE FIELDS ( SWIFTCode LongBankName )
    WITH VALUE #( ( %key-BankCountry      = bank_country
                    %key-BankInternalID   = bank_internal_id
                    %is_draft             = if_abap_behv=>mk-on
                    SWIFTCode             = swift_code
                    LongBankName          = long_bank_name
                    %control-SWIFTCode    = if_abap_behv=>mk-on
                    %control-LongBankName = if_abap_behv=>mk-on ) )
    FAILED   DATA(failed)
    REPORTED DATA(reported).
  IF failed IS INITIAL.
    COMMIT ENTITIES.
    rv_success = abap_true.
  ENDIF.
ENDMETHOD.
```
This helper is called before **Activate** and before exiting edit mode with **Keep Draft**, so nothing the user typed is silently dropped.

#### 7. Exiting Edit Mode — Compare Draft vs Active
Switching back to VIEW asks: did the user actually change anything? If not, just discard the draft silently. If yes, show a popup so the user explicitly chooses Keep or Discard.
```abap
METHOD has_changes_vs_active.
  READ ENTITIES OF i_banktp
    ENTITY Bank
    FIELDS ( SWIFTCode LongBankName )
    WITH VALUE #( ( %key-BankCountry    = bank_country
                    %key-BankInternalID = bank_internal_id
                    %is_draft           = if_abap_behv=>mk-off ) )
    RESULT DATA(actives).
  IF actives IS INITIAL.
    rv_changed = abap_true.
    RETURN.
  ENDIF.
  rv_changed = xsdbool( swift_code     <> actives[ 1 ]-SWIFTCode
                     OR long_bank_name <> actives[ 1 ]-LongBankName ).
ENDMETHOD.
```
`on_event_edit_toggle` uses this to decide between a silent discard and the **Keep Draft / Discard Draft** popup. Picking **Keep Draft** runs `save_current_to_draft( )` and leaves the draft on the server for later. Picking **Discard Draft** executes the `Discard` action, releasing the lock and reverting the inputs to the active record.

#### 8. Activating — Promote the Draft to Active
Activation is two steps: first save the currently typed values into the draft, then run the `Activate` action. On success the database row is updated and the app returns to VIEW mode.
```abap
METHOD on_event_activate.
  IF save_current_to_draft( ) = abap_false.
    status_text = `Could not save draft before activation — see messages below.`.
    view_display( ).
    RETURN.
  ENDIF.

  MODIFY ENTITIES OF i_banktp
    ENTITY Bank
    EXECUTE Activate
      FROM VALUE #( ( %cid                = 'ACT_BANK'
                      %key-BankCountry    = bank_country
                      %key-BankInternalID = bank_internal_id ) )
    FAILED   DATA(failed)
    REPORTED DATA(reported).

  IF failed IS INITIAL.
    COMMIT ENTITIES.
    mode       = `VIEW`.
    draft_open = abap_false.
    read_active( ).
    client->message_toast_display( `Bank activated successfully.` ).
  ENDIF.
  view_display( ).
ENDMETHOD.
```

#### 9. The Event Map and the View
The dispatcher in `z2ui5_if_app~main` wires UI events to the methods above:
```abap
IF client->check_on_init( ).
  on_init( ).
ELSEIF client->check_on_event( `EDIT_TOGGLE` ).
  on_event_edit_toggle( ).
ELSEIF client->check_on_event( `ACTIVATE` ).
  on_event_activate( ).
ELSEIF client->check_on_event( `STARTUP_RESUME` ).
  on_event_startup_resume( ).
ELSEIF client->check_on_event( `STARTUP_DISCARD` ).
  on_event_startup_discard( ).
ELSEIF client->check_on_event( `EXIT_KEEP` ).
  on_event_exit_keep( ).
ELSEIF client->check_on_event( `EXIT_DISCARD` ).
  on_event_exit_discard( ).
ENDIF.
```
The view itself is a single `simple_form` whose `editable` and per-input `enabled` flags follow `draft_open`. The primary button label and type flip between **Switch to Edit Mode** and **Switch to View Mode** based on `mode`, and **Activate** is only enabled while a draft is open.

#### Full Snippet

::: details Full working example — `z2ui5_cl_sample_draft`
```abap
CLASS z2ui5_cl_sample_draft DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA bank_country     TYPE c LENGTH 3  VALUE `DE`.
    DATA bank_internal_id TYPE c LENGTH 15 VALUE `50070010`.
    DATA swift_code       TYPE c LENGTH 11.
    DATA long_bank_name   TYPE string.
    DATA draft_open       TYPE abap_bool.
    DATA mode             TYPE string VALUE `VIEW`.   " VIEW or EDIT
    DATA status_text      TYPE string.
    DATA messages         TYPE string.
  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS on_init.
    METHODS on_event_edit_toggle.
    METHODS on_event_activate.
    METHODS on_event_startup_resume.
    METHODS on_event_startup_discard.
    METHODS on_event_exit_keep.
    METHODS on_event_exit_discard.
    METHODS on_event_leave_app.
    METHODS check_existing_draft.
    METHODS draft_acquire.
    METHODS draft_resume.
    METHODS draft_read.
    METHODS save_current_to_draft
      RETURNING VALUE(rv_success) TYPE abap_bool.
    METHODS has_changes_vs_active
      RETURNING VALUE(rv_changed) TYPE abap_bool.
    METHODS read_active.
    METHODS exit_edit_mode.
    METHODS view_display.
ENDCLASS.

CLASS z2ui5_cl_sample_draft IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `EDIT_TOGGLE` ).
      on_event_edit_toggle( ).
    ELSEIF client->check_on_event( `ACTIVATE` ).
      on_event_activate( ).
    ELSEIF client->check_on_event( `STARTUP_RESUME` ).
      on_event_startup_resume( ).
    ELSEIF client->check_on_event( `STARTUP_DISCARD` ).
      on_event_startup_discard( ).
    ELSEIF client->check_on_event( `EXIT_KEEP` ).
      on_event_exit_keep( ).
    ELSEIF client->check_on_event( `EXIT_DISCARD` ).
      on_event_exit_discard( ).
    ELSEIF client->check_on_event( `LEAVE_APP` ).
      on_event_leave_app( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_init.
    " App opens in VIEW mode showing the active database record
    mode       = `VIEW`.
    draft_open = abap_false.
    read_active( ).
    status_text = `View mode — click Edit to make changes.`.
    view_display( ).
  ENDMETHOD.

  METHOD on_event_edit_toggle.
    IF mode = `VIEW`.
      " VIEW → EDIT: check for an existing draft (popup if found)
      check_existing_draft( ).
      RETURN.
    ENDIF.

    " EDIT → VIEW
    IF has_changes_vs_active( ) = abap_false.
      " No changes vs active record — just discard the draft silently
      CLEAR messages.
      MODIFY ENTITIES OF i_banktp
        ENTITY Bank
        EXECUTE Discard
          FROM VALUE #( ( %key-BankCountry    = bank_country
                          %key-BankInternalID = bank_internal_id ) )
        FAILED   DATA(failed)
        REPORTED DATA(reported).
      COMMIT ENTITIES.

      exit_edit_mode( ).
      client->message_toast_display( `No changes — exited edit mode.` ).
      view_display( ).
      RETURN.
    ENDIF.

    " Changes detected — ask whether to keep or discard
    client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory(
        i_title               = `Exit Edit Mode`
        i_question_text       = `You have unsaved changes. Keep the draft for later, or discard it now?`
        i_icon                = `sap-icon://question-mark`
        i_button_text_confirm = `Keep Draft`
        i_button_text_cancel  = `Discard Draft`
        i_event_confirm       = `EXIT_KEEP`
        i_event_cancel        = `EXIT_DISCARD` ) ).
  ENDMETHOD.

  METHOD has_changes_vs_active.
    READ ENTITIES OF i_banktp
      ENTITY Bank
      FIELDS ( SWIFTCode LongBankName )
      WITH VALUE #( ( %key-BankCountry    = bank_country
                      %key-BankInternalID = bank_internal_id
                      %is_draft           = if_abap_behv=>mk-off ) )
      RESULT DATA(actives).

    IF actives IS INITIAL.
      rv_changed = abap_true.   " defensive: treat unknown active as change
      RETURN.
    ENDIF.

    IF swift_code     = actives[ 1 ]-SWIFTCode AND
       long_bank_name = actives[ 1 ]-LongBankName.
      rv_changed = abap_false.
    ELSE.
      rv_changed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD on_event_exit_keep.
    " Persist any unsaved typed values, then exit edit mode (draft stays server-side)
    IF save_current_to_draft( ) = abap_false.
      status_text = `Could not save draft on exit — see messages below.`.
      view_display( ).
      RETURN.
    ENDIF.
    exit_edit_mode( ).
    client->message_toast_display( `Draft saved — edit mode ended.` ).
    view_display( ).
  ENDMETHOD.

  METHOD on_event_exit_discard.
    " Discard the draft, then exit edit mode
    CLEAR messages.
    MODIFY ENTITIES OF i_banktp
      ENTITY Bank
      EXECUTE Discard
        FROM VALUE #( ( %key-BankCountry    = bank_country
                        %key-BankInternalID = bank_internal_id ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    LOOP AT reported-bank ASSIGNING FIELD-SYMBOL(<bank>).
      IF <bank>-%msg IS BOUND.
        messages = |{ messages }• { <bank>-%msg->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.
    LOOP AT reported-%other ASSIGNING FIELD-SYMBOL(<other>).
      IF <other> IS BOUND.
        messages = |{ messages }• { <other>->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.

    IF failed IS NOT INITIAL.
      status_text = `Discard failed — see messages below.`.
      view_display( ).
      RETURN.
    ENDIF.
    COMMIT ENTITIES.

    exit_edit_mode( ).
    status_text = `Draft discarded — original values restored. View mode.`.
    client->message_toast_display( `Draft discarded.` ).
    view_display( ).
  ENDMETHOD.

  METHOD exit_edit_mode.
    mode       = `VIEW`.
    draft_open = abap_false.
    read_active( ).
    status_text = `View mode — click Edit to make changes.`.
  ENDMETHOD.

  METHOD read_active.
    READ ENTITIES OF i_banktp
      ENTITY Bank
      FIELDS ( SWIFTCode LongBankName )
      WITH VALUE #( ( %key-BankCountry    = bank_country
                      %key-BankInternalID = bank_internal_id
                      %is_draft           = if_abap_behv=>mk-off ) )
      RESULT DATA(actives).
    IF actives IS NOT INITIAL.
      swift_code     = actives[ 1 ]-SWIFTCode.
      long_bank_name = actives[ 1 ]-LongBankName.
    ENDIF.
  ENDMETHOD.

  METHOD check_existing_draft.
    SELECT SINGLE a~created_by, a~last_changed_at
      FROM cabnk_bank_d AS d
      INNER JOIN sdraft_admin AS a
        ON a~draft_key = d~draftadministrativedatauuid
      WHERE d~bankcountry    = @bank_country
        AND d~bankinternalid = @bank_internal_id
      INTO @DATA(draft_info).

    IF sy-subrc <> 0.
      " No existing draft — create a fresh one and switch to EDIT
      draft_acquire( ).
      draft_read( ).
      mode = `EDIT`.
      view_display( ).
      RETURN.
    ENDIF.

    DATA(lv_owner)     = draft_info-created_by.
    DATA(lv_timestamp) = draft_info-last_changed_at.
    DATA(lv_time_text) = |{ lv_timestamp TIMESTAMP = ISO }|.

    IF lv_owner = sy-uname.
      " Same user — popup with Resume / Discard choice
      client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory(
          i_title               = `Existing Draft Found`
          i_question_text       = |You have an open draft (last changed: { lv_time_text }). | &
                                  |Resume working on it, or discard and start fresh?|
          i_icon                = `sap-icon://edit`
          i_button_text_confirm = `Resume Draft`
          i_button_text_cancel  = `Discard && Start New`
          i_event_confirm       = `STARTUP_RESUME`
          i_event_cancel        = `STARTUP_DISCARD` ) ).
    ELSE.
      " Different user holds the draft — stay in VIEW mode
      mode        = `VIEW`.
      draft_open  = abap_false.
      status_text = |Draft locked by user { lv_owner } since { lv_time_text }. | &
                    |Cannot enter edit mode.|.
      view_display( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_event_startup_resume.
    draft_resume( ).
    draft_read( ).
    mode = `EDIT`.
    client->message_toast_display( `Edit mode — resumed existing draft.` ).
    view_display( ).
  ENDMETHOD.

  METHOD on_event_startup_discard.
    MODIFY ENTITIES OF i_banktp
      ENTITY Bank
      EXECUTE Discard
        FROM VALUE #( ( %key-BankCountry    = bank_country
                        %key-BankInternalID = bank_internal_id ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).
    COMMIT ENTITIES.

    client->message_toast_display( `Old draft discarded — starting fresh.` ).
    draft_acquire( ).
    draft_read( ).
    mode = `EDIT`.
    view_display( ).
  ENDMETHOD.

  METHOD draft_acquire.
    CLEAR messages.
    MODIFY ENTITIES OF i_banktp
      ENTITY Bank
      EXECUTE Edit
        FROM VALUE #( ( %cid                    = 'EDIT_BANK'
                        %key-BankCountry        = bank_country
                        %key-BankInternalID     = bank_internal_id
                        %param-preserve_changes = abap_true ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    LOOP AT reported-bank ASSIGNING FIELD-SYMBOL(<bank>).
      IF <bank>-%msg IS BOUND.
        messages = |{ messages }• { <bank>-%msg->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.
    LOOP AT reported-%other ASSIGNING FIELD-SYMBOL(<other>).
      IF <other> IS BOUND.
        messages = |{ messages }• { <other>->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.

    IF failed IS NOT INITIAL.
      draft_open  = abap_false.
      status_text = `Could not open draft — see messages below.`.
      RETURN.
    ENDIF.
    COMMIT ENTITIES.
    draft_open  = abap_true.
    status_text = `Edit mode — new draft open. Save or Activate to commit.`.
  ENDMETHOD.

  METHOD draft_resume.
    CLEAR messages.
    MODIFY ENTITIES OF i_banktp
      ENTITY Bank
      EXECUTE Resume
        FROM VALUE #( ( %key-BankCountry    = bank_country
                        %key-BankInternalID = bank_internal_id ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    LOOP AT reported-bank ASSIGNING FIELD-SYMBOL(<bank>).
      IF <bank>-%msg IS BOUND.
        messages = |{ messages }• { <bank>-%msg->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.
    LOOP AT reported-%other ASSIGNING FIELD-SYMBOL(<other>).
      IF <other> IS BOUND.
        messages = |{ messages }• { <other>->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.

    IF failed IS NOT INITIAL.
      draft_open  = abap_false.
      status_text = `Could not resume draft — see messages below.`.
      RETURN.
    ENDIF.
    COMMIT ENTITIES.
    draft_open  = abap_true.
    status_text = `Edit mode — resumed existing draft.`.
  ENDMETHOD.

  METHOD draft_read.
    IF draft_open = abap_false.
      RETURN.
    ENDIF.
    READ ENTITIES OF i_banktp
      ENTITY Bank
      FIELDS ( SWIFTCode LongBankName )
      WITH VALUE #( ( %key-BankCountry    = bank_country
                      %key-BankInternalID = bank_internal_id
                      %is_draft           = if_abap_behv=>mk-on ) )
      RESULT DATA(drafts).
    IF drafts IS NOT INITIAL.
      swift_code     = drafts[ 1 ]-SWIFTCode.
      long_bank_name = drafts[ 1 ]-LongBankName.
    ENDIF.
  ENDMETHOD.

  METHOD save_current_to_draft.
    CLEAR messages.
    MODIFY ENTITIES OF i_banktp
      ENTITY Bank
      UPDATE FIELDS ( SWIFTCode LongBankName )
      WITH VALUE #( ( %key-BankCountry      = bank_country
                      %key-BankInternalID   = bank_internal_id
                      %is_draft             = if_abap_behv=>mk-on
                      SWIFTCode             = swift_code
                      LongBankName          = long_bank_name
                      %control-SWIFTCode    = if_abap_behv=>mk-on
                      %control-LongBankName = if_abap_behv=>mk-on ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    LOOP AT reported-bank ASSIGNING FIELD-SYMBOL(<bank>).
      IF <bank>-%msg IS BOUND.
        messages = |{ messages }• { <bank>-%msg->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.
    LOOP AT reported-%other ASSIGNING FIELD-SYMBOL(<other>).
      IF <other> IS BOUND.
        messages = |{ messages }• { <other>->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.

    IF failed IS NOT INITIAL.
      rv_success = abap_false.
      RETURN.
    ENDIF.
    COMMIT ENTITIES.
    rv_success = abap_true.
  ENDMETHOD.

  METHOD on_event_activate.
    " First persist any unsaved typed values to the draft
    IF save_current_to_draft( ) = abap_false.
      status_text = `Could not save draft before activation — see messages below.`.
      view_display( ).
      RETURN.
    ENDIF.

    MODIFY ENTITIES OF i_banktp
      ENTITY Bank
      EXECUTE Activate
        FROM VALUE #( ( %cid                = 'ACT_BANK'
                        %key-BankCountry    = bank_country
                        %key-BankInternalID = bank_internal_id ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    LOOP AT reported-bank ASSIGNING FIELD-SYMBOL(<bank>).
      IF <bank>-%msg IS BOUND.
        messages = |{ messages }• { <bank>-%msg->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.
    LOOP AT reported-%other ASSIGNING FIELD-SYMBOL(<other>).
      IF <other> IS BOUND.
        messages = |{ messages }• { <other>->if_message~get_text( ) }\n|.
      ENDIF.
    ENDLOOP.

    IF failed IS NOT INITIAL.
      status_text = `Activation failed — see messages below.`.
      view_display( ).
      RETURN.
    ENDIF.
    COMMIT ENTITIES.

    " Switch back to VIEW mode and show success
    mode        = `VIEW`.
    draft_open  = abap_false.
    read_active( ).
    status_text = `Bank activated successfully — your changes are now in the database. View mode.`.
    client->message_toast_display( `Bank activated successfully.` ).
    view_display( ).
  ENDMETHOD.

  METHOD on_event_leave_app.
    client->nav_app_leave( ).
  ENDMETHOD.

  METHOD view_display.
    DATA(lv_edit_button_text) = COND string(
        WHEN mode = `EDIT` THEN `Switch to View Mode`
        ELSE `Switch to Edit Mode` ).
    DATA(lv_edit_button_type) = COND string(
        WHEN mode = `VIEW` THEN `Emphasized`
        ELSE `Default` ).
    DATA(lv_form_title) = COND string(
        WHEN mode = `EDIT` THEN `Bank header (draft)`
        ELSE `Bank header (view)` ).

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Bank — Standard BO Draft via EML`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event( `LEAVE_APP` )
            )->simple_form(
                title    = lv_form_title
                editable = draft_open
                )->content( `form`
                )->label( `Bank Country`
                )->input(
                    value   = bank_country
                    enabled = abap_false
                )->label( `Bank Key`
                )->input(
                    value   = bank_internal_id
                    enabled = abap_false
                )->label( `Bank Name`
                )->input(
                    value   = client->_bind( long_bank_name )
                    enabled = draft_open
                )->label( `SWIFT Code`
                )->input(
                    value   = client->_bind( swift_code )
                    enabled = draft_open
                )->label( `Status`
                )->input(
                    value   = status_text
                    enabled = abap_false
                )->label( `Messages`
                )->text_area(
                    value    = messages
                    rows     = `6`
                    editable = abap_false
                )->button(
                    text  = lv_edit_button_text
                    type  = lv_edit_button_type
                    press = client->_event( `EDIT_TOGGLE` )
                )->button(
                    text    = `Activate`
                    type    = `Emphasized`
                    press   = client->_event( `ACTIVATE` )
                    enabled = draft_open ).
    client->view_display( view->stringify( ) ).
  ENDMETHOD.
ENDCLASS.
```
:::

## Common Pitfalls

A quick checklist of the mistakes beginners hit most often:

| Symptom | Cause | Fix |
|---|---|---|
| Activate saves the *old* values | Typed values were never pushed into the draft | Call `UPDATE FIELDS` **before** `Activate` (step 6) |
| Nothing is persisted at all | Missing `COMMIT ENTITIES` | EML stays in the buffer until you commit |
| Read returns the active record, not the draft | Wrong `%is_draft` flag | Use `mk-on` to read the draft, `mk-off` for the active record |
| "Record is locked" errors | A leftover draft from a previous session | `Resume` or `Discard` the existing draft (step 2) |
| Changes silently lost on exit | No save before leaving edit mode | Save (or prompt to keep) before switching back to VIEW |

For the full story on inspecting `FAILED` / `REPORTED` after EML calls, see the **Failure Handling** section in [EML](./eml.md).

::: tip
The field names (`BankCountry`, `BankInternalID`, `SWIFTCode`, `LongBankName`) and the draft-shadow table (`cabnk_bank_d`) match the released `I_BankTP` on current S/4HANA — on other releases the BO name, fields, or shadow table may differ. If no standard BO covers your object, define your own draft-enabled RAP BO (with its own `draft table z…_d`, `lock master`, etc.) and consume it the same way; see the [SAP RAP draft documentation](https://help.sap.com/docs/abap-cloud/abap-rap/draft). If you need locks for non-draft objects, see [Locks](../expert_more/lock.md).
:::
