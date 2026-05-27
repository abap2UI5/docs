---
outline: [2, 4]
---
# Draft Handling

Draft handling lets users save unfinished work — for example a half-filled form — without writing through to the active database state. In abap2UI5 apps you drive RAP draft-enabled business objects directly through EML, just like any other entity (see also [EML](./eml.md)).

#### Create a Draft
Use `MODIFY ENTITIES` with `%is_draft = if_abap_behv=>mk-on` to create a new draft instance:
```abap
METHOD z2ui5_if_app~main.

  MODIFY ENTITIES OF i_salesordertp
         ENTITY salesorder
         CREATE
         FIELDS ( salesordertype salesorganization )
         WITH VALUE #( ( %cid      = `0001`
                         %is_draft = if_abap_behv=>mk-on
                         %data     = VALUE #(
                             SalesOrderType    = `TA`
                             SalesOrganization = `1010` ) ) )
         MAPPED   DATA(ls_mapped)
         FAILED   DATA(ls_failed)
         REPORTED DATA(ls_reported).

  COMMIT ENTITIES.

ENDMETHOD.
```

#### Read a Draft
Set `%is_draft = if_abap_behv=>mk-on` to read the draft instead of the active record:
```abap
READ ENTITIES OF i_salesordertp
  ENTITY SalesOrder
  FIELDS ( SalesOrderType )
  WITH VALUE #( ( %key-SalesOrder = `0000004711`
                  %is_draft       = if_abap_behv=>mk-on ) )
  RESULT DATA(lt_drafts).
```

#### Activate or Discard
Promote the draft to the active state with the `Activate` action, or throw it away with `Discard`:
```abap
MODIFY ENTITIES OF i_salesordertp
       ENTITY SalesOrder
       EXECUTE Activate
       FROM VALUE #( ( %key-SalesOrder     = `0000004711`
                       %key-IsActiveEntity = abap_false ) )
       FAILED   DATA(ls_failed)
       REPORTED DATA(ls_reported).

COMMIT ENTITIES.
```

### Editing a Standard SAP Draft BO

On S/4HANA or BTP ABAP Environment (Steampunk), many business objects already ship as draft-enabled BOs (e.g. `I_BankTP`, `I_SalesOrderTP`). You don't build a BO and you don't create a draft table — SAP provides both. Your abap2UI5 app just calls the standard BO via EML, which sidesteps the whole lock-during-think-time problem.

The session can stay **stateless**: the draft survives between roundtrips in SAP's draft-shadow table, and the lock is held by the BO framework as long as the draft exists. Closing the browser without activating or discarding leaves the draft for the same user to resume later — no `set_session_stateful( )`, no `ENQUEUE_*`, no custom Z table.

The walkthrough below uses `I_BankTP` to edit a bank record. The flow is split into two modes:
- **VIEW mode** — read-only display of the active database record. No lock, no draft.
- **EDIT mode** — a draft is open; inputs are editable. The BO framework holds the lock.

Toggling between modes drives the entire draft lifecycle (acquire, resume, save, activate, discard).

#### 1. App Startup — Always Begin in View Mode
On `on_init` the app reads the active record and renders it read-only. No draft is touched yet, so other users can still edit the same record.
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
`%is_draft = if_abap_behv=>mk-off` makes the read return the active row, not any open draft.

#### 2. Entering Edit Mode — Check for an Existing Draft
When the user clicks **Switch to Edit Mode**, the app first looks in the BO's draft-shadow table (here `cabnk_bank_d`) joined with `sdraft_admin` to find out whether a draft already exists for this key — and who owns it.
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

The shadow-table name (`cabnk_bank_d`) is BO-specific; check `Draft Table` in the behavior definition for your own BO.

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
After a successful `Edit`, the lock is held by the BO framework and a row exists in the draft-shadow table. `draft_open = abap_true` switches the view's inputs from read-only to editable.

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
The fields are two-way bound (`client->_bind_edit( … )`), so user input lives in ABAP variables on the next roundtrip. To persist them in the draft, push them back with `UPDATE FIELDS`.
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
                    value   = client->_bind_edit( long_bank_name )
                    enabled = draft_open
                )->label( `SWIFT Code`
                )->input(
                    value   = client->_bind_edit( swift_code )
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

::: tip
The field names (`BankCountry`, `BankInternalID`, `SWIFTCode`, `LongBankName`) and the draft-shadow table (`cabnk_bank_d`) match the released `I_BankTP` on current S/4HANA — on other releases the BO name, fields, or shadow table may differ. If no standard BO covers your object, define your own draft-enabled RAP BO (with its own `draft table z…_d`, `lock master`, etc.) and consume it the same way; see the [SAP RAP draft documentation](https://help.sap.com/docs/abap-cloud/abap-rap/draft). If you need locks for non-draft objects, see [Locks](../expert_more/lock.md).
:::
