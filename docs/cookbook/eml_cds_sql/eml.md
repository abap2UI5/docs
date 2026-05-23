---
outline: [2, 4]
---
# EML

All examples in these docs work without EML. But on a recent ABAP release, you can also use this feature in abap2UI5 apps.

### EML
The Entity Manipulation Language simplifies work with RAP business objects by giving a consistent way to read, create, update, and delete entities.

#### Read
Use `READ ENTITIES` to fetch sales orders and show them in a UI5 table:
```abap
CLASS z2ui5_cl_sample_eml_read DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    DATA mt_salesorder TYPE TABLE FOR READ RESULT i_salesordertp\\salesorder.

ENDCLASS.

CLASS z2ui5_cl_sample_eml_read IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    IF client->check_on_init( ).

      READ ENTITIES OF I_SalesOrderTP
        ENTITY SalesOrder
        ALL FIELDS WITH
        VALUE #( ( SalesOrder = `0000000001` ) )
        RESULT mt_salesorder.

      DATA(view) = z2ui5_cl_xml_view=>factory( )->page( ).
      DATA(table) = view->table( client->_bind( mt_salesorder ) ).
      table->columns(
           )->column( )->text( `SalesOrder` )->get_parent(
           )->column( )->text( `SalesOrderType` )->get_parent(
           )->column( )->text( `SalesOrganization` ).

      table->items( )->column_list_item( )->cells(
         )->text( `{SALESORDER}`
         )->text( `{SALESORDERTYPE}`
         )->text( `{SALESORGANIZATION}` ).

      client->view_display( view->stringify( ) ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
```

#### Modify
The example below creates a sales order with `MODIFY` inside an abap2UI5 app:

```abap
  METHOD z2ui5_if_app~main.

    MODIFY ENTITIES OF i_salesordertp
           ENTITY salesorder
           CREATE
           FIELDS ( salesordertype
                    salesorganization
                    distributionchannel
                    organizationdivision
                    soldtoparty )
           WITH VALUE #( ( %cid  = `0001`
                           %data = VALUE #(
                SalesOrderType       = `TA`
                SalesOrganization    = `1010`
                DistributionChannel  = `10`
                OrganizationDivision = `00`
                SoldToParty          = `0033500056` ) ) )
           MAPPED   DATA(ls_mapped)
           FAILED   DATA(ls_failed)
           REPORTED DATA(ls_reported_modify).

    COMMIT ENTITIES BEGIN
           RESPONSE OF i_salesordertp
           FAILED   DATA(ls_save_failed)
           REPORTED DATA(ls_save_reported).
    COMMIT ENTITIES END.

ENDMETHOD.
```
Key Points:
- EML calls in abap2UI5 apps run outside the RAP framework, so explicit transaction commits (COMMIT ENTITIES) are needed.
- Restrictions inside the RAP framework, like disallowing direct calls to posting function modules or explicit commits, don't apply to abap2UI5 EML operations. You get more flexibility when handling commits and other actions.

### Failure Handling

EML statements (`MODIFY ENTITIES`, `READ ENTITIES`, `COMMIT ENTITIES`) report problems through the `FAILED` and `REPORTED` structures rather than by raising exceptions. They can also raise classic ABAP exceptions for infrastructure-level failures.

What to handle:
- **Business / validation failures** — inspect `FAILED` (which entities failed) and `REPORTED` (the messages explaining why) after each EML call. These are *not* exceptions; an unchecked `FAILED` will look like success in your code while the data was never written.
- **Transactional behavior** — EML modifications stay in the transactional buffer until `COMMIT ENTITIES`. If you skip the commit, nothing is persisted. If `COMMIT ENTITIES` itself reports failures, you must decide whether to retry, roll back, or surface the error to the user.
- **Infrastructure exceptions** — wrap EML calls in `TRY … CATCH` for the cases that *do* raise:
  - `cx_root` / `cx_dynamic_check` — catch-all safety net.
  - `cx_abap_invalid_value`, `cx_sy_conversion_no_number` — data conversion problems before the EML statement runs.
  - `cx_abap_behv` and its subclasses — RAP behavior framework errors (e.g. unknown action, locking issues).
  - `cx_abap_lock_failure` — when `COMMIT ENTITIES` cannot acquire locks.

A typical defensive pattern:

```abap
TRY.
    MODIFY ENTITIES OF z_i_invoice
      ENTITY invoice
      UPDATE FIELDS ( amount ) WITH VALUE #( ( %tky = ls_key amount = lv_amount ) )
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    IF failed IS NOT INITIAL.
      " surface reported messages, do NOT commit
      RAISE EXCEPTION NEW cx_abap_behv( ).
    ENDIF.

    COMMIT ENTITIES RESPONSE OF z_i_invoice
      FAILED   DATA(commit_failed)
      REPORTED DATA(commit_reported).

    IF commit_failed IS NOT INITIAL.
      RAISE EXCEPTION NEW cx_abap_behv( ).
    ENDIF.

  CATCH cx_root INTO DATA(lx).
    client->nav_app_call( z2ui5_cl_pop_error=>factory( lx ) ).
ENDTRY.
```

For general exception handling and the framework's error popup, see the [Exception](/cookbook/event_navigation/exception) page.
