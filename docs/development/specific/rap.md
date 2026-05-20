---
outline: [2, 4]
---
# RAP

### RAP is a Programming Model — abap2UI5 is Not

**RAP (RESTful Application Programming Model)** is a full-stack programming model. It prescribes how you design, expose, and consume business objects: you define entities with CDS views, declare their behavior in Behavior Definitions (BDEFs), implement handlers in Behavior Implementation classes, and expose everything as an OData V4 service consumed by a Fiori Elements frontend. Every layer is part of the model.

**abap2UI5 is not a programming model.** It is a UI rendering library — a single ABAP interface your class implements. It has no opinion on how you structure your data, which access layer you use, or how your business logic is organized. There is no CDS requirement, no BDEF, no OData service. abap2UI5 simply calls your `main` method on each user interaction and renders whatever view you return.

This distinction matters: RAP defines the architecture *around* your application. abap2UI5 only defines how the UI is built *inside* your ABAP class.

### abap2UI5 is Agnostic

Because abap2UI5 imposes no access layer, you pick whatever fits:

| Access Layer | Works in abap2UI5? |
|---|---|
| ABAP SQL (`SELECT`, `INSERT`, …) | ✅ |
| CDS Views (VDM, custom) | ✅ |
| EML (`READ ENTITIES`, `MODIFY ENTITIES`) | ✅ |
| Function modules, BAPIs | ✅ |
| RAP actions called via EML | ✅ |
| Direct table access | ✅ |

Nothing is excluded. The abap2UI5 controller is a plain ABAP class — any statement that is valid in ABAP is valid there.

### Using RAP Functionality from Outside

When you call EML from inside the RAP framework (e.g., from a Behavior Implementation), the framework enforces its own rules: no explicit `COMMIT WORK`, no direct database modifications, controlled side effects.

abap2UI5 runs **outside** the RAP framework. This means all RAP restrictions that apply inside the framework do not apply here. You call EML freely:

```abap
READ ENTITIES OF i_salesordertp
  ENTITY salesorder
  ALL FIELDS WITH VALUE #( ( SalesOrder = `0000000001` ) )
  RESULT DATA(lt_result).
```

```abap
MODIFY ENTITIES OF i_salesordertp
  ENTITY salesorder
  CREATE FIELDS ( salesordertype salesorganization soldtoparty )
  WITH VALUE #( ( %cid = `001` %data = VALUE #( ... ) ) )
  MAPPED DATA(ls_mapped)
  FAILED DATA(ls_failed)
  REPORTED DATA(ls_reported).

COMMIT ENTITIES BEGIN
  RESPONSE OF i_salesordertp
  FAILED DATA(ls_save_failed)
  REPORTED DATA(ls_save_reported).
COMMIT ENTITIES END.
```

The explicit `COMMIT ENTITIES` is required — and fully permitted — because you are not inside a RAP handler. The same applies to reading CDS views, calling RAP actions, or accessing draft tables directly.

::: tip
Running outside the RAP framework gives you more control: you decide when to commit, how to handle errors, and how to combine multiple business object operations in one user interaction.
:::
