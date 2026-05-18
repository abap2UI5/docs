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

::: tip
A draft-enabled BO holds an exclusive lock for its owner as long as the draft exists, so the app can stay stateless and the user can resume work later. If you need additional locks for non-draft objects, see [Locks](./locks.md).
:::
