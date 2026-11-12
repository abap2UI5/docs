---
outline: [2, 2]
description: The full source of the six lock strategies of the Lock page, one class each - no lock, lock at save, optimistic check, the two combined, a stateful session and a soft lock table.
---
# Lock: Full Source

The [Lock](/cookbook/expert_more/lock) page explains eight ways to keep two
users from overwriting each other, each with a short snippet. This page holds
the complete class for the first six. Every class works on the sales order
header table `VBAK` and its enqueue object `EVVBAK`.

## 1. No Locking {#lock-1}

The class `ZCL_SAMPLE_LOCK_1`, explained under [1. No Locking](/cookbook/expert_more/lock#_1-no-locking).

<!-- playground: no Run button — SELECTs from VBAK, which no browser database has -->
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

CLASS zcl_sample_lock_1 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.
    DATA ernam TYPE vbak-ernam.
    DATA erdat TYPE vbak-erdat.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sample_lock_1 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
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

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title`          v = `Edit Sales Order — No Locking`
                    )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
                    )->a( n = `showNavButton`  b = client->check_app_prev_stack( )

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `Header`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `Sales Order`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( vbeln )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Label`
                                )->a( n = `text` v = `Type`
                            )->tag( `Input`
                                )->a( n = `value`   v = client->_bind( auart )
                            )->tag( `Label`
                                )->a( n = `text` v = `Created by`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( ernam )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Label`
                                )->a( n = `text` v = `Created on`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( erdat )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Button`
                                )->a( n = `text`    v = `Save`
                                )->a( n = `press`   v = client->_event( `SAVE` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

## 2. Lock at Save {#lock-2}

The class `ZCL_SAMPLE_LOCK_2`, explained under [2. Lock at Save](/cookbook/expert_more/lock#_2-lock-at-save).

<!-- playground: no Run button — calls the ENQUEUE_EVVBAK function module, and there is no application server -->
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

CLASS zcl_sample_lock_2 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sample_lock_2 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
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

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title`          v = `Edit Sales Order — Enqueue at Save`
                    )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
                    )->a( n = `showNavButton`  b = client->check_app_prev_stack( )

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `Header`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `Sales Order`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( vbeln )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Label`
                                )->a( n = `text` v = `Type`
                            )->tag( `Input`
                                )->a( n = `value`   v = client->_bind( auart )
                            )->tag( `Button`
                                )->a( n = `text`    v = `Save`
                                )->a( n = `press`   v = client->_event( `SAVE` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

## 3. Optimistic Check {#lock-3}

The class `ZCL_SAMPLE_LOCK_3`, explained under [3. Optimistic Check](/cookbook/expert_more/lock#_3-optimistic-check).

<!-- playground: no Run button — SELECTs from VBAK, which no browser database has -->
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

CLASS zcl_sample_lock_3 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.

    DATA token_aedat TYPE vbak-aedat.
    DATA token_tmstmp TYPE vbak-UPD_TMSTMP.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sample_lock_3 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
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
      INTO ( @auart, @token_aedat, @token_tmstmp ).

  ENDMETHOD.


  METHOD on_event_save.

    DATA current_aedat TYPE vbak-aedat.
    DATA current_tmstmp TYPE vbak-UPD_TMSTMP.

    SELECT SINGLE aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @current_aedat, @current_tmstmp ).

    IF current_aedat <> token_aedat OR current_tmstmp <> token_tmstmp.
      client->message_box_display(
        |Sales order { vbeln } has been changed by another user since you opened it. Please refresh.| ).
      RETURN.
    ENDIF.

    "don't do that, just for demo
    "GET TIME STAMP FIELD now.
    "UPDATE vbak
    "  SET auart = @auart,
    "      aedat = @sy-datum,
    "      UPD_TMSTMP = @now
    "  WHERE vbeln = @vbeln.
    "COMMIT WORK.

    data_read( ).
    client->message_toast_display( `Saved.` ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title`          v = `Edit Sales Order — Optimistic Locking`
                    )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
                    )->a( n = `showNavButton`  b = client->check_app_prev_stack( )

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `Header`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `Sales Order`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( vbeln )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Label`
                                )->a( n = `text` v = `Type`
                            )->tag( `Input`
                                )->a( n = `value`   v = client->_bind( auart )
                            )->tag( `Button`
                                )->a( n = `text`    v = `Save`
                                )->a( n = `press`   v = client->_event( `SAVE` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

## 4. Combined {#lock-4}

The class `ZCL_SAMPLE_LOCK_4`, explained under [4. Combined](/cookbook/expert_more/lock#_4-combined-recommended-default).

<!-- playground: no Run button — calls the ENQUEUE_EVVBAK function module, and there is no application server -->
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

CLASS zcl_sample_lock_4 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.

    DATA token_aedat TYPE vbak-aedat.
    DATA token_tmstmp TYPE vbak-UPD_TMSTMP.

    METHODS on_init.
    METHODS on_event_save.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sample_lock_4 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
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
      INTO ( @auart, @token_aedat, @token_tmstmp ).

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
    DATA current_tmstmp TYPE vbak-UPD_TMSTMP.

    SELECT SINGLE aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @current_aedat, @current_tmstmp ).

    IF current_aedat <> token_aedat OR current_tmstmp <> token_tmstmp.

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
    "GET TIME STAMP FIELD now.
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
    client->message_toast_display( `Saved.` ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title`          v = `Edit Sales Order — Enqueue + Optimistic`
                    )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
                    )->a( n = `showNavButton`  b = client->check_app_prev_stack( )

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `Header`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `Sales Order`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( vbeln )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Label`
                                )->a( n = `text` v = `Type`
                            )->tag( `Input`
                                )->a( n = `value`   v = client->_bind( auart )
                            )->tag( `Button`
                                )->a( n = `text`    v = `Save`
                                )->a( n = `press`   v = client->_event( `SAVE` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

## 5. Stateful Session {#lock-5}

The class `ZCL_SAMPLE_LOCK_5`, explained under [5. Stateful Session](/cookbook/expert_more/lock#_5-stateful-session).

<!-- playground: no Run button — calls the ENQUEUE_EVVBAK function module, and there is no application server -->
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

CLASS zcl_sample_lock_5 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    DATA vbeln TYPE vbak-vbeln VALUE `0000004711`.

    METHODS on_init.
    METHODS on_event_save.
    METHODS on_event_cancel.
    METHODS lock_acquire.
    METHODS lock_release.
    METHODS view_display.
    METHODS data_read.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_sample_lock_5 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
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
    "GET TIME STAMP FIELD now.
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

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title`          v = `Edit Sales Order — Stateful Lock`
                    )->a( n = `navButtonPress` v = client->_event( `CANCEL` )
                    )->a( n = `showNavButton`  b = client->check_app_prev_stack( )

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `Header`
                        )->a( n = `editable` b = abap_true

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `Sales Order`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( vbeln )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Label`
                                )->a( n = `text` v = `Type`
                            )->tag( `Input`
                                )->a( n = `value`   v = client->_bind( auart )
                            )->tag( `Button`
                                )->a( n = `text`    v = `Save`
                                )->a( n = `press`   v = client->_event( `SAVE` )
                            )->tag( `Button`
                                )->a( n = `text`    v = `Cancel`
                                )->a( n = `press`   v = client->_event( `CANCEL` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

## 6. Soft Lock {#lock-6}

The class `ZCL_SAMPLE_LOCK_6`, explained under [6. Soft Lock](/cookbook/expert_more/lock#_6-soft-lock).

<!-- playground: no Run button — calls the ENQUEUE_EVVBAK function module, and there is no application server -->
```abap
* Scenario 6 — Soft lock (advisory only)
*
* A soft lock is a row in a custom Z table marking
* "user X is editing sales order Y." It is NOT enforced by the SAP
* kernel — only your app code respects it. Use it for UX feedback
* ("locked by Alice since 09:32"), always layered on top of a real
* save-time guard.
*
* Required Z table Z2UI5_SAMPLE_01 (create via SE11):
*   MANDT      MANDT       Client (key)
*   VBELN      VBELN_VA    Sales order (key)
*   USERNAME   SYUNAME     Editing user
*   LOCKED_AT  TIMESTAMPL  When the lock started
* Delivery class A.

CLASS zcl_sample_lock_6 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA auart TYPE vbak-auart.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    DATA vbeln       TYPE vbak-vbeln VALUE `0000004711`.
    DATA locked_by   TYPE string.
    DATA token_aedat TYPE vbak-aedat.
    DATA token_tmstmp TYPE vbak-UPD_TMSTMP.

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


CLASS zcl_sample_lock_6 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.
    IF client->check_on_navigated( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE` ).
      on_event_save( ).
    ELSEIF client->check_on_event( `RELEASE` ).
      on_event_release( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_init.

    " soft_lock_acquire fills locked_by when someone else holds the lock;
    " the view renders read-only in that case
    soft_lock_acquire( ).
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
    GET TIME STAMP FIELD s_new-locked_at.

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
      INTO ( @auart, @token_aedat, @token_tmstmp ).

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
    DATA current_tmstmp TYPE vbak-UPD_TMSTMP.

    SELECT SINGLE aedat, UPD_TMSTMP
      FROM vbak
      WHERE vbeln = @vbeln
      INTO ( @current_aedat, @current_tmstmp ).

    IF current_aedat <> token_aedat OR current_tmstmp <> token_tmstmp.

      CALL FUNCTION 'DEQUEUE_EVVBAK'
        EXPORTING
          mode_vbak = `E`
          mandt     = sy-mandt
          vbeln     = vbeln.

      client->message_box_display( `Record changed by another user. Please refresh.` ).
      RETURN.

    ENDIF.

    "don't do that, just demo
    "GET TIME STAMP FIELD now.
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

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`      v = `sap.m`
            )->a( n = `xmlns:mvc`  v = `sap.ui.core.mvc`
            )->a( n = `xmlns:form` v = `sap.ui.layout.form`

            )->ele( `Shell`
                )->ele( `Page`
                    )->a( n = `title`          v = `Edit Sales Order — Soft Lock + Save Guard`
                    )->a( n = `navButtonPress` v = client->_event_nav_app_leave( )
                    )->a( n = `showNavButton`  b = client->check_app_prev_stack( )

                    )->ele( n = `SimpleForm` ns = `form`
                        )->a( n = `title`    v = `Header`
                        )->a( n = `editable` b = editable

                        )->ele( n = `content` ns = `form`
                            )->tag( `Label`
                                )->a( n = `text` v = `Sales Order`
                            )->tag( `Input`
                                )->a( n = `value`   t = CONV string( vbeln )
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Label`
                                )->a( n = `text` v = `Type`
                            )->tag( `Input`
                                )->a( n = `value`   v = client->_bind( auart )
                            )->tag( `Label`
                                )->a( n = `text` v = `Status`
                            )->tag( `Input`
                                )->a( n = `value`   t = locked_by
                                )->a( n = `enabled` b = abap_false
                            )->tag( `Button`
                                )->a( n = `text`    v = `Save`
                                )->a( n = `press`   v = client->_event( `SAVE` )
                                )->a( n = `enabled` b = editable
                            )->tag( `Button`
                                )->a( n = `text`    v = `Release & Exit`
                                )->a( n = `press`   v = client->_event( `RELEASE` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```
