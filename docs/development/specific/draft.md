---
outline: [2, 4]
---
# Draft Handling

Draft handling lets users save unfinished work — for example, a half-filled form — without committing it to the active database state. With abap2UI5 you can drive RAP draft-enabled business objects directly through EML.

### Modify a Draft
Use `MODIFY ENTITIES` with the `IN LOCAL MODE` and the draft addition to create or update draft instances:
```abap
MODIFY ENTITIES OF i_salesordertp
       ENTITY salesorder
       CREATE
       FIELDS ( salesordertype salesorganization )
       WITH VALUE #( ( %cid     = `0001`
                       %is_draft = if_abap_behv=>mk-on
                       %data     = VALUE #(
                           SalesOrderType    = `TA`
                           SalesOrganization = `1010` ) ) )
       MAPPED   DATA(ls_mapped)
       FAILED   DATA(ls_failed)
       REPORTED DATA(ls_reported).
```

### Activate a Draft
Promote the draft to the active state with the `Activate` action and commit:
```abap
MODIFY ENTITIES OF i_salesordertp
       ENTITY salesorder
       EXECUTE Activate
       FROM VALUE #( ( %key       = ls_key
                       %is_draft  = if_abap_behv=>mk-on ) )
       MAPPED   DATA(ls_mapped_act)
       FAILED   DATA(ls_failed_act)
       REPORTED DATA(ls_reported_act).

COMMIT ENTITIES BEGIN
       RESPONSE OF i_salesordertp
       FAILED   DATA(ls_save_failed)
       REPORTED DATA(ls_save_reported).
COMMIT ENTITIES END.
```

For more on EML in abap2UI5 apps, see [CDS, EML](./cds.md).

::: tip
Draft tables hold exclusive locks for the draft owner. Combine draft handling with [Locks](./locks.md) only when you also need backend lock objects outside of RAP.
:::
