---
outline: [2, 4]
---
# Draft Handling

Draft handling lets users save unfinished work — for example a half-filled form — without writing through to the active database state. In abap2UI5 apps you drive RAP draft-enabled business objects directly through EML, just like any other entity (see also [CDS, EML](./cds.md)).

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
Set `%key-IsActiveEntity = abap_false` to read the draft instead of the active record:
```abap
READ ENTITIES OF i_salesordertp
  ENTITY SalesOrder
  FIELDS ( SalesOrderType )
  WITH VALUE #( ( %key-SalesOrder     = `0000004711`
                  %key-IsActiveEntity = abap_false ) )
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

#### Editing a Standard SAP Draft BO
On S/4HANA or BTP ABAP Environment (Steampunk), many business objects already ship as draft-enabled BOs (e.g. `I_SalesOrderTP`). You don't build a BO and you don't create a draft table — SAP provides both. Your abap2UI5 app just calls the standard BO via EML, which sidesteps the whole lock-during-think-time problem.

The session can stay **stateless**: the draft survives between roundtrips in SAP's draft-shadow table, and the lock is held by the BO framework as long as the draft exists. Closing the browser without activating or discarding leaves the draft for the same user to resume later — no `set_session_stateful( )`, no `ENQUEUE_*`, no custom Z table.

The user flow:
```
on_init              -> Edit       (create or resume the draft)
"Save Draft" pressed -> UPDATE     (writes the draft only, VBAK stays as-is)
"Activate" pressed   -> Activate   (now VBAK is written through)
"Discard" pressed    -> Discard    (drops the draft, releases lock)
```

A complete example based on `I_SalesOrderTP`:
```abap
CLASS z2ui5_cl_sample_draft DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA sales_order      TYPE c LENGTH 10 VALUE `0000004711`.
    DATA sales_order_type TYPE c LENGTH 4.
    DATA draft_open       TYPE abap_bool.
    DATA status_text      TYPE string.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_init.
    METHODS on_event_save_draft.
    METHODS on_event_activate.
    METHODS on_event_discard.
    METHODS draft_acquire.
    METHODS draft_read.
    METHODS view_display.

ENDCLASS.


CLASS z2ui5_cl_sample_draft IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
    ELSEIF client->check_on_event( `SAVE_DRAFT` ).
      on_event_save_draft( ).
    ELSEIF client->check_on_event( `ACTIVATE` ).
      on_event_activate( ).
    ELSEIF client->check_on_event( `DISCARD` ).
      on_event_discard( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.
    draft_acquire( ).
    draft_read( ).
    view_display( ).
  ENDMETHOD.

  METHOD draft_acquire.

    " Edit is idempotent: if a draft already exists, SAP returns it.
    MODIFY ENTITIES OF i_salesordertp
      ENTITY SalesOrder
      EXECUTE Edit
        FROM VALUE #( ( %key-SalesOrder        = sales_order
                        %param-PreserveChanges = abap_true ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    IF failed IS NOT INITIAL.
      draft_open  = abap_false.
      status_text = `Could not open draft — locked by another user, or no authorization.`.
      RETURN.
    ENDIF.

    COMMIT ENTITIES.

    draft_open  = abap_true.
    status_text = `Draft open — changes are saved as a draft until you press Activate.`.

  ENDMETHOD.

  METHOD draft_read.

    IF draft_open = abap_false.
      RETURN.
    ENDIF.

    READ ENTITIES OF i_salesordertp
      ENTITY SalesOrder
      FIELDS ( SalesOrderType )
      WITH VALUE #( ( %key-SalesOrder     = sales_order
                      %key-IsActiveEntity = abap_false ) )
      RESULT DATA(drafts).

    IF drafts IS NOT INITIAL.
      sales_order_type = drafts[ 1 ]-SalesOrderType.
    ENDIF.

  ENDMETHOD.

  METHOD on_event_save_draft.

    MODIFY ENTITIES OF i_salesordertp
      ENTITY SalesOrder
      UPDATE FIELDS ( SalesOrderType )
      WITH VALUE #( ( %tky-SalesOrder         = sales_order
                      %tky-IsActiveEntity     = abap_false
                      SalesOrderType          = sales_order_type
                      %control-SalesOrderType = if_abap_behv=>mk-on ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    IF failed IS NOT INITIAL.
      client->message_box_display( `Draft update failed.` ).
      RETURN.
    ENDIF.

    COMMIT ENTITIES.
    client->message_toast_display( `Draft saved.` ).

  ENDMETHOD.

  METHOD on_event_activate.

    MODIFY ENTITIES OF i_salesordertp
      ENTITY SalesOrder
      EXECUTE Activate
        FROM VALUE #( ( %key-SalesOrder     = sales_order
                        %key-IsActiveEntity = abap_false ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    IF failed IS NOT INITIAL.
      client->message_box_display( `Activation failed — see application log.` ).
      RETURN.
    ENDIF.

    COMMIT ENTITIES.
    client->message_toast_display( `Sales order activated.` ).
    client->nav_app_leave( ).

  ENDMETHOD.

  METHOD on_event_discard.

    MODIFY ENTITIES OF i_salesordertp
      ENTITY SalesOrder
      EXECUTE Discard
        FROM VALUE #( ( %key-SalesOrder     = sales_order
                        %key-IsActiveEntity = abap_false ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    COMMIT ENTITIES.
    client->nav_app_leave( ).

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    view->shell(
        )->page(
            title          = `Edit Sales Order — Standard BO Draft via EML`
            shownavbutton  = client->check_app_prev_stack( )
            navbuttonpress = client->_event( `DISCARD` )
            )->simple_form(
                title    = `Header (draft)`
                editable = draft_open
                )->content( `form`
                )->label( `Sales Order`
                )->input(
                    value   = sales_order
                    enabled = abap_false
                )->label( `Type`
                )->input( client->_bind_edit( sales_order_type )
                )->label( `Status`
                )->input(
                    value   = status_text
                    enabled = abap_false
                )->button(
                    text    = `Save Draft`
                    press   = client->_event( `SAVE_DRAFT` )
                    enabled = draft_open
                )->button(
                    text    = `Activate`
                    type    = `Emphasized`
                    press   = client->_event( `ACTIVATE` )
                    enabled = draft_open
                )->button(
                    text  = `Discard`
                    press = client->_event( `DISCARD` ) ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
```

::: tip
The field names (`SalesOrder`, `SalesOrderType`) match the released `I_SalesOrderTP` on current S/4HANA — on older releases the BO name or fields may differ. If no standard BO covers your object, define your own draft-enabled RAP BO (with its own `draft table z…_d`, `lock master`, etc.) and consume it the same way; see the [SAP RAP draft documentation](https://help.sap.com/docs/abap-cloud/abap-rap/draft). If you need locks for non-draft objects, see [Locks](./locks.md).
:::
