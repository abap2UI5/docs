---
outline: [2, 4]
---
# Lock

In classic SAP GUI, a transaction like `VA02` calls `ENQUEUE_EVVBAK` and the lock lives as long as the dialog session. Web apps are different: each roundtrip is a fresh ABAP session by default, so a lock set in one roundtrip is gone by the next. Locking in abap2UI5 means picking a deliberate strategy for two questions:

| Phase | Question |
|---|---|
| **Edit** | What happens while the user is thinking and typing? |
| **Save** | What happens the moment they hit save? |

The patterns below combine in different ways. The examples use the sales order header table `VBAK` and its standard enqueue object `EVVBAK`, but the same shapes apply to any table. All snippets have a complete, copy-paste-ready demo class — sample numbers are linked next to each section.

#### 1. No Locking
The minimal starting point — the user edits and saves, no lock and no conflict check. Last save wins, silently. Fine for personal sandboxes and throwaway demos, but rarely what you want in production. See sample `Z2UI5_CL_DEMO_APP_S_07`.

<details>
<summary>Full source — <code>Z2UI5_CL_DEMO_APP_S_07</code></summary>

```abap
* Scenario 1 — Naive editing (no locking)
*
* The simplest possible starting point. The user can change a sales
* order and save. There is no lock and no conflict check. Last save
* wins, silently.
*
* When to use this:
*   - Personal sandboxes, throwaway demos, internal tools where only
*     one user ever touches a record.

CLASS z2ui5_cl_demo_app_s_07 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA vbeln    TYPE vbak-vbeln VALUE `0000004711`.
    DATA auart    TYPE vbak-auart.
    DATA ernam    TYPE vbak-ernam.
    DATA erdat    TYPE vbak-erdat.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_demo_app_s_07 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE` ).
      on_event_save( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_init.

    data_read( ).
    view_display( ).

  ENDMETHOD.


  METHOD data_read.

    SELECT SINGLE auart, ernam, erdat
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @auart, @ernam, @erdat ).

  ENDMETHOD.


  METHOD on_event_save.

    "don't do that, just demo
    "UPDATE vbak SET auart = @auart WHERE vbeln = @vbeln.
    "COMMIT WORK.

    client->message_toast_display( `Saved.` ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Sales Order — No Locking`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event_nav_app_leave( )
            )->simple_form(
                title    = `Header`
                editable = abap_true
                )->content( `form`
                )->label( `Sales Order`
                )->input(
                    value   = vbeln
                    enabled = abap_false
                )->label( `Type`
                )->input( client->_bind_edit( auart )
                )->label( `Created by`
                )->input(
                    value   = ernam
                    enabled = abap_false
                )->label( `Created on`
                )->input(
                    value   = CONV string( erdat )
                    enabled = abap_false
                )->button(
                    text  = `Save`
                    press = client->_event( `SAVE` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

</details>

#### 2. Lock at Save
Do not hold a lock while the user thinks. Acquire it the moment they save, write, commit, and release — all in one short roundtrip:
```abap
METHOD on_event_save.

  CALL FUNCTION `ENQUEUE_EVVBAK`
    EXPORTING
      mode_vbak      = `E`
      mandt          = sy-mandt
      vbeln          = vbeln
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    client->message_box_display( `Locked by another user.` ).
    RETURN.
  ENDIF.

  UPDATE vbak SET auart = @auart WHERE vbeln = @vbeln.
  COMMIT WORK.

  CALL FUNCTION `DEQUEUE_EVVBAK`
    EXPORTING
      mode_vbak = `E`
      mandt     = sy-mandt
      vbeln     = vbeln.

ENDMETHOD.
```

The lock exists for milliseconds, so this scales — but two parallel saves can still race if the timestamps line up, and the second one silently overwrites the first. See sample `Z2UI5_CL_DEMO_APP_S_08`.

<details>
<summary>Full source — <code>Z2UI5_CL_DEMO_APP_S_08</code></summary>

```abap
* Scenario 2 — Edit + Enqueue at save
*
* We do NOT hold a lock while the user thinks. At the moment they
* press Save, we lock, write, commit, and release in one short
* roundtrip.
*
* When to use this:
*   - Quick edits with low chance of two users hitting the same record
*   - Default starting point for most stateless editing apps

CLASS z2ui5_cl_demo_app_s_08 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.
    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_demo_app_s_08 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE` ).
      on_event_save( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_init.

    data_read( ).
    view_display( ).

  ENDMETHOD.


  METHOD data_read.

    SELECT SINGLE auart
      FROM vbak
      WHERE vbeln = @vbeln
      INTO @auart.

  ENDMETHOD.


  METHOD on_event_save.

    CALL FUNCTION 'ENQUEUE_EVVBAK'
      EXPORTING
        mode_vbak      = `E`
        mandt          = sy-mandt
        vbeln          = vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      client->message_box_display( |Cannot lock { vbeln } — already locked by another user| ).
      RETURN.
    ENDIF.

    "don't do that, just demo
    "UPDATE vbak SET auart = @auart WHERE vbeln = @vbeln.
    "COMMIT WORK.

    CALL FUNCTION 'DEQUEUE_EVVBAK'
      EXPORTING
        mode_vbak = `E`
        mandt     = sy-mandt
        vbeln     = vbeln.

    client->message_toast_display( `Saved.` ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Sales Order — Enqueue at Save`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event_nav_app_leave( )
            )->simple_form(
                title    = `Header`
                editable = abap_true
                )->content( `form`
                )->label( `Sales Order`
                )->input(
                    value   = vbeln
                    enabled = abap_false
                )->label( `Type`
                )->input( client->_bind_edit( auart )
                )->button(
                    text  = `Save`
                    press = client->_event( `SAVE` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

</details>

#### 3. Optimistic Check
On read, remember the record's change timestamp. On save, re-read and reject if it shifted in the meantime — the same idea as an HTTP ETag:
```abap
METHOD data_read.
  SELECT SINGLE aedat, UPD_TMSTMP FROM vbak
    INTO ( @token_aedat, @token_aezet )
    WHERE vbeln = @vbeln.
ENDMETHOD.

METHOD on_event_save.

  SELECT SINGLE aedat, UPD_TMSTMP FROM vbak
    INTO ( @DATA(now_aedat), @DATA(now_aezet) )
    WHERE vbeln = @vbeln.

  IF now_aedat <> token_aedat OR now_aezet <> token_aezet.
    client->message_box_display( `Record changed — please reload.` ).
    RETURN.
  ENDIF.

  " ...write through...

ENDMETHOD.
```

No lock is held during the edit phase, so this scales to many concurrent users and catches conflicts even from outside your app (SE16, batch jobs, RFC). See sample `Z2UI5_CL_DEMO_APP_S_09`.

Pick a column that *always* updates on writes. If anyone writes the table bypassing `AEDAT` / `UPD_TMSTMP`, the check silently misses real conflicts.

<details>
<summary>Full source — <code>Z2UI5_CL_DEMO_APP_S_09</code></summary>

```abap
* Scenario 3 — Optimistic locking (timestamp check)
*
* On read we remember the record's last-changed timestamp. On save,
* we re-read it and reject if it changed in the meantime. Same idea
* as HTTP ETag or OData's @odata.etag.
*
* When to use this:
*   - Any time silent overwrites would be a problem
*   - Combine with Scenario 2 — it costs almost nothing and catches
*     real bugs

CLASS z2ui5_cl_demo_app_s_09 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.
    DATA auart TYPE vbak-auart.

    DATA token_aedat TYPE vbak-aedat.
    DATA token_aezet TYPE vbak-UPD_TMSTMP.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_demo_app_s_09 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE` ).
      on_event_save( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_init.

    data_read( ).
    view_display( ).

  ENDMETHOD.


  METHOD data_read.

    SELECT SINGLE auart, aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @auart, @token_aedat, @token_aezet ).

  ENDMETHOD.


  METHOD on_event_save.

    DATA current_aedat TYPE vbak-aedat.
    DATA current_aezet TYPE vbak-UPD_TMSTMP.

    SELECT SINGLE aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @current_aedat, @current_aezet ).

    IF current_aedat <> token_aedat OR current_aezet <> token_aezet.
      client->message_box_display(
        |Sales order { vbeln } has been changed by another user since you opened it. Please refresh.| ).
      RETURN.
    ENDIF.

    "don't do that, just for demo
    "DATA(now) = z2ui5_cl_util=>time_get_timestampl( ).
    "UPDATE vbak
    "  SET auart = @auart,
    "      aedat = @sy-datum,
    "      UPD_TMSTMP = @now
    "  WHERE vbeln = @vbeln.
    "COMMIT WORK.

    data_read( ).
    client->view_model_update( ).
    client->message_toast_display( `Saved.` ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Sales Order — Optimistic Locking`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event_nav_app_leave( )
            )->simple_form(
                title    = `Header`
                editable = abap_true
                )->content( `form`
                )->label( `Sales Order`
                )->input(
                    value   = vbeln
                    enabled = abap_false
                )->label( `Type`
                )->input( client->_bind_edit( auart )
                )->button(
                    text  = `Save`
                    press = client->_event( `SAVE` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

</details>

#### 4. Combined (recommended default)
**Lock at Save** plus the **Optimistic Check** is the safest stateless pattern and the sensible production default — the enqueue serializes parallel saves of *this* app, the timestamp check catches everyone else. See sample `Z2UI5_CL_DEMO_APP_S_10`.

<details>
<summary>Full source — <code>Z2UI5_CL_DEMO_APP_S_10</code></summary>

```abap
* Scenario 4 — Combined (the recommended default)
*
* Enqueue at save AND the optimistic check. The safest stateless
* pattern, and the one most production apps should default to.
*
* Why both:
*   - The enqueue serializes concurrent writers so two saves cannot
*     interleave.
*   - The timestamp check catches anyone who slipped in via another
*     path (SE16, batch job, classic SAP GUI) between read and write.

CLASS z2ui5_cl_demo_app_s_10 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.
    DATA auart TYPE vbak-auart.

    DATA token_aedat TYPE vbak-aedat.
    DATA token_aezet TYPE vbak-UPD_TMSTMP.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_demo_app_s_10 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE` ).
      on_event_save( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_init.

    data_read( ).
    view_display( ).

  ENDMETHOD.


  METHOD data_read.

    SELECT SINGLE auart, aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @auart, @token_aedat, @token_aezet ).

  ENDMETHOD.


  METHOD on_event_save.

    CALL FUNCTION 'ENQUEUE_EVVBAK'
      EXPORTING
        mode_vbak      = `E`
        mandt          = sy-mandt
        vbeln          = vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      client->message_box_display( |Sales order { vbeln } is currently being saved by another user. Try again.| ).
      RETURN.
    ENDIF.

    DATA current_aedat TYPE vbak-aedat.
    DATA current_aezet TYPE vbak-UPD_TMSTMP.

    SELECT SINGLE aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @current_aedat, @current_aezet ).

    IF current_aedat <> token_aedat OR current_aezet <> token_aezet.

      CALL FUNCTION 'DEQUEUE_EVVBAK'
        EXPORTING
          mode_vbak = `E`
          mandt     = sy-mandt
          vbeln     = vbeln.

      client->message_box_display(
        |Sales order { vbeln } has been changed by another user since you opened it. Please refresh.| ).
      RETURN.

    ENDIF.

    "don't do that, just for demo
    "DATA(now) = z2ui5_cl_util=>time_get_timestampl( ).
    "UPDATE vbak
    "  SET auart = @auart,
    "      aedat = @sy-datum,
    "      UPD_TMSTMP = @now
    "  WHERE vbeln = @vbeln.
    "COMMIT WORK.

    CALL FUNCTION 'DEQUEUE_EVVBAK'
      EXPORTING
        mode_vbak = `E`
        mandt     = sy-mandt
        vbeln     = vbeln.

    data_read( ).
    client->view_model_update( ).
    client->message_toast_display( `Saved.` ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Sales Order — Enqueue + Optimistic`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event_nav_app_leave( )
            )->simple_form(
                title    = `Header`
                editable = abap_true
                )->content( `form`
                )->label( `Sales Order`
                )->input(
                    value   = vbeln
                    enabled = abap_false
                )->label( `Type`
                )->input( client->_bind_edit( auart )
                )->button(
                    text  = `Save`
                    press = client->_event( `SAVE` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

</details>

#### 5. Stateful Session
For classic SAP GUI-like behaviour, switch the session to stateful and call the lock function module on init. The lock survives subsequent roundtrips as long as the session stays alive:
```abap
IF client->check_on_init( ).

  CALL FUNCTION `ENQUEUE_EVVBAK`
    EXPORTING
      mode_vbak      = `E`
      mandt          = sy-mandt
      vbeln          = vbeln
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc = 0.
    client->set_session_stateful( ).
  ELSE.
    client->message_box_display( `Locked by another user.` ).
    client->nav_app_leave( ).
    RETURN.
  ENDIF.

ENDIF.
```

While the app is open, an entry for `EVVBAK` / `0000004711` is visible in SM12. The user can navigate, type, wait — the lock stays. On exit, release it explicitly:
```abap
CALL FUNCTION `DEQUEUE_EVVBAK`
  EXPORTING
    mode_vbak = `E`
    mandt     = sy-mandt
    vbeln     = vbeln.
client->set_session_stateful( abap_false ).
```

See samples `Z2UI5_CL_DEMO_APP_S_11` and `Z2UI5_CL_DEMO_APP_350` for complete examples.

::: warning
Each active user pins a work process. Use stateful sessions only for low-traffic, internal apps. Always pair `set_session_stateful( )` with an explicit `set_session_stateful( abap_false )` on every exit path — otherwise a leaked enqueue blocks future users until the session times out.
:::

<details>
<summary>Full source — <code>Z2UI5_CL_DEMO_APP_S_11</code></summary>

```abap
* Scenario 5 — Stateful session with a persistent enqueue
*
* Classic SAP GUI behaviour: the moment the user opens the screen the
* lock is held until they save or leave. With set_session_stateful( ),
* abap2UI5 keeps the session alive between roundtrips so a real
* ENQUEUE_EVVBAK survives.
*
* When to use this:
*   - Internal back-office apps with few concurrent users
*   - You want users to immediately see "locked by X" when opening
*
* Cost: each active user pins a work process. Do NOT use this for
* high-traffic apps.

CLASS z2ui5_cl_demo_app_s_11 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.
    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event_save.
    METHODS on_event_cancel.
    METHODS lock_acquire.
    METHODS lock_release.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_demo_app_s_11 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE` ).
      on_event_save( ).
    ELSEIF client->check_on_event( `CANCEL` ).
      on_event_cancel( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_init.

    lock_acquire( ).

  ENDMETHOD.


  METHOD lock_acquire.

    CALL FUNCTION 'ENQUEUE_EVVBAK'
      EXPORTING
        mode_vbak      = `E`
        mandt          = sy-mandt
        vbeln          = vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.

      client->set_session_stateful( abap_false ).
      client->message_box_display( |Sales order { vbeln } is locked by another user.| ).
      RETURN.

    ENDIF.

    client->set_session_stateful( ).
    data_read( ).
    view_display( ).

  ENDMETHOD.


  METHOD lock_release.

    CALL FUNCTION 'DEQUEUE_EVVBAK'
      EXPORTING
        mode_vbak = `E`
        mandt     = sy-mandt
        vbeln     = vbeln.

    client->set_session_stateful( abap_false ).

  ENDMETHOD.


  METHOD data_read.

    SELECT SINGLE auart
      FROM vbak
      WHERE vbeln = @vbeln
      INTO @auart.

  ENDMETHOD.


  METHOD on_event_save.

    "don't do that, just demo
    "DATA(now) = z2ui5_cl_util=>time_get_timestampl( ).
    "UPDATE vbak
    "  SET auart = @auart,
    "      aedat = @sy-datum,
    "      UPD_TMSTMP = @now
    "  WHERE vbeln = @vbeln.
    "COMMIT WORK.

    lock_release( ).
    client->message_toast_display( `Saved.` ).
    client->nav_app_leave( ).

  ENDMETHOD.


  METHOD on_event_cancel.

    lock_release( ).
    client->nav_app_leave( ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Sales Order — Stateful Lock`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event( `CANCEL` )
            )->simple_form(
                title    = `Header`
                editable = abap_true
                )->content( `form`
                )->label( `Sales Order`
                )->input(
                    value   = vbeln
                    enabled = abap_false
                )->label( `Type`
                )->input( client->_bind_edit( auart )
                )->button(
                    text  = `Save`
                    press = client->_event( `SAVE` )
                )->button(
                    text  = `Cancel`
                    press = client->_event( `CANCEL` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

</details>

#### 6. Soft Lock
A soft lock is a row in a custom Z table marking *"user X is editing object Y"*. It is **not** enforced by the SAP kernel — only your app code respects it — so use it for UX feedback ("locked by Alice since 09:32") and always layer it on top of a real save-time guard. A minimal schema:

| Field      | Type        | Description           |
|------------|-------------|-----------------------|
| MANDT      | MANDT       | Client (key)          |
| VBELN      | VBELN_VA    | Object (key)          |
| USERNAME   | SYUNAME     | Editing user          |
| LOCKED_AT  | TIMESTAMPL  | When the lock started |

A user closing the browser without pressing *Release* leaves the row behind, so add a cleanup job that deletes entries older than, say, 30 minutes. See sample `Z2UI5_CL_DEMO_APP_S_12` (with the matching `Z2UI5_SAMPLE_01` table).

<details>
<summary>Full source — <code>Z2UI5_CL_DEMO_APP_S_12</code></summary>

```abap
* Scenario 6 — Soft lock (advisory only)
*
* A soft lock is a row in a custom Z table marking
* "user X is editing sales order Y." It is NOT enforced by the SAP
* kernel — only your app code respects it. Use it for UX feedback
* ("locked by Alice since 09:32"), always layered on top of a real
* save-time guard.
*
* Required Z table ZS_SO_LOCK (create via SE11):
*   MANDT      MANDT       Client (key)
*   VBELN      VBELN_VA    Sales order (key)
*   USERNAME   SYUNAME     Editing user
*   LOCKED_AT  TIMESTAMPL  When the lock started
* Delivery class A.

CLASS z2ui5_cl_demo_app_s_12 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA vbeln       TYPE vbak-vbeln VALUE `0000004711`.
    DATA auart       TYPE vbak-auart.
    DATA locked_by   TYPE string.
    DATA token_aedat TYPE vbak-aedat.
    DATA token_aezet TYPE vbak-UPD_TMSTMP.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event_save.
    METHODS on_event_release.
    METHODS soft_lock_acquire
      RETURNING
        VALUE(ok) TYPE abap_bool.
    METHODS soft_lock_release.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_demo_app_s_12 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE` ).
      on_event_save( ).
    ELSEIF client->check_on_event( `RELEASE` ).
      on_event_release( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_init.

    IF soft_lock_acquire( ) = abap_false.

      data_read( ).
      view_display( ).
      RETURN.

    ENDIF.

    data_read( ).
    view_display( ).

  ENDMETHOD.


  METHOD soft_lock_acquire.

    DATA s_existing TYPE z2ui5_sample_01.

    SELECT SINGLE *
      FROM z2ui5_sample_01
      WHERE vbeln = @vbeln
      INTO @s_existing.

    IF sy-subrc = 0 AND s_existing-username <> sy-uname.

      locked_by = |Locked by { s_existing-username } since { s_existing-locked_at TIMESTAMP = USER }|.
      ok        = abap_false.
      RETURN.

    ENDIF.

    DATA s_new TYPE z2ui5_sample_01.
    s_new-vbeln    = vbeln.
    s_new-username = sy-uname.
    s_new-locked_at = z2ui5_cl_util=>time_get_timestampl( ).

    MODIFY z2ui5_sample_01 FROM @s_new.
    COMMIT WORK.

    locked_by = ``.
    ok       = abap_true.

  ENDMETHOD.


  METHOD soft_lock_release.

    DELETE FROM z2ui5_sample_01
      WHERE vbeln    = @vbeln
        AND username = @sy-uname.
    COMMIT WORK.

  ENDMETHOD.


  METHOD data_read.

    SELECT SINGLE auart, aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @auart, @token_aedat, @token_aezet ).

  ENDMETHOD.


  METHOD on_event_save.

    CALL FUNCTION 'ENQUEUE_EVVBAK'
      EXPORTING
        mode_vbak      = `E`
        mandt          = sy-mandt
        vbeln          = vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      client->message_box_display( `Could not acquire enqueue` ).
      RETURN.
    ENDIF.

    DATA current_aedat TYPE vbak-aedat.
    DATA current_aezet TYPE vbak-UPD_TMSTMP.

    SELECT SINGLE aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @current_aedat, @current_aezet ).

    IF current_aedat <> token_aedat OR current_aezet <> token_aezet.

      CALL FUNCTION 'DEQUEUE_EVVBAK'
        EXPORTING
          mode_vbak = `E`
          mandt     = sy-mandt
          vbeln     = vbeln.

      client->message_box_display( `Record changed by another user. Please refresh.` ).
      RETURN.

    ENDIF.

    "don't do that, just demo
    "DATA(now) = z2ui5_cl_util=>time_get_timestampl( ).
    "UPDATE vbak
    "  SET auart = @auart,
    "      aedat = @sy-datum,
    "      UPD_TMSTMP = @now
    "  WHERE vbeln = @vbeln.
    "COMMIT WORK.

    CALL FUNCTION 'DEQUEUE_EVVBAK'
      EXPORTING
        mode_vbak = `E`
        mandt     = sy-mandt
        vbeln     = vbeln.

    soft_lock_release( ).
    client->message_toast_display( `Saved.` ).
    client->nav_app_leave( ).

  ENDMETHOD.


  METHOD on_event_release.

    soft_lock_release( ).
    client->nav_app_leave( ).

  ENDMETHOD.


  METHOD view_display.

    DATA(editable) = COND abap_bool( WHEN locked_by IS INITIAL THEN abap_true ELSE abap_false ).

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Sales Order — Soft Lock + Save Guard`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event_nav_app_leave( )
            )->simple_form(
                title    = `Header`
                editable = editable
                )->content( `form`
                )->label( `Sales Order`
                )->input(
                    value   = vbeln
                    enabled = abap_false
                )->label( `Type`
                )->input( client->_bind_edit( auart )
                )->label( `Status`
                )->input(
                    value   = locked_by
                    enabled = abap_false
                )->button(
                    text    = `Save`
                    press   = client->_event( `SAVE` )
                    enabled = editable
                )->button(
                    text  = `Release & Exit`
                    press = client->_event( `RELEASE` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

</details>

#### 7. RAP Drafts
On modern releases, RAP draft-enabled business objects manage locking for you: the draft holds an exclusive lock for its owner while the user keeps editing — no stateful session, no `ENQUEUE_*` call. If a released SAP BO already covers your object, this is usually the simplest path. See [Draft Handling](../eml_cds_sql/draft_handling.md).

#### 8. Lock-Manager Add-on
The community add-on [**lock-manager**](https://github.com/abap2UI5-addons/lock-manager) wraps the lock logic in a reusable class — including stale-lock cleanup and a "locked by X since…" message for the user. Install it like any other [add-on](../../resources/addons.md) and call it instead of writing the boilerplate yourself.

#### Overview

| # | Strategy | "Locked by X" while editing | Catches external writes | Stateless | Best fit |
|---|---|---|---|---|---|
| 1 | No Locking | no | no | yes | Demos, sandboxes |
| 2 | Lock at Save | no | partial (race window) | yes | Single-app writers, low contention |
| 3 | Optimistic Check | no | yes (timestamp) | yes | Many concurrent users |
| 4 | Combined | no | yes | yes | **Production default** |
| 5 | Stateful Session | yes (enqueue) | yes | no (pins work process) | GUI-like feel, few users |
| 6 | Soft Lock | yes (Z table) | only via underlying guard | yes | "Locked by Alice" UX |
| 7 | RAP Drafts | yes (RAP-managed) | yes | yes | A released draft-enabled BO exists |
| 8 | Lock-Manager Add-on | yes | yes | yes | Skip the boilerplate |

Start with **(4) Combined** unless one of these tips the balance:
- The app is read-only → no lock needed
- A released, draft-enabled SAP BO already covers your object → **(7) RAP Drafts**
- You need a "locked by X since…" message at open → add **(6) Soft Lock** on top of (4), or use **(5) Stateful Session** for few users with a GUI-like feel
- A lock-manager add-on exists for your platform → **(8) Lock-Manager Add-on**

For the underlying concepts and trade-offs of statefulness, see [Statefulness](./statefulness.md).
