---
outline: [2, 4]
samples:
  - z2ui5_cl_smps_app_313
  - z2ui5_cl_smps_app_319
  - z2ui5_cl_smps_app_475
  - z2ui5_cl_smps_app_476
  - z2ui5_cl_smps_app_477
  - z2ui5_cl_smps_app_478
  - z2ui5_cl_smps_app_479
  - z2ui5_cl_smps_app_493
---
# Smart Controls

Smart controls from the `sap.ui.comp` library (SmartFilterBar, SmartTable, SmartForm, SmartField, SmartChart) build their UI from **OData V2 metadata** — combined with variant management, they give you a full list report without hand-built columns or filters.

::: warning SAPUI5 only
The `sap.ui.comp` library ships with SAPUI5 but not with OpenUI5 — apps using smart controls require a SAPUI5 bootstrap. See [UI5 Bootstrapping](/configuration/setup/ui5_bootstrapping).
:::

## Declaring the Namespaces

Smart controls live in their own `sap.ui.comp` sub-namespaces, so each one you use needs its `xmlns` on the root `View` — exactly as in a hand-written UI5 view. The three below cover a list report:

```abap
    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`                        v = `sap.m`
            )->a( n = `xmlns:mvc`                    v = `sap.ui.core.mvc`
            )->a( n = `xmlns:smartFilterBar`         v = `sap.ui.comp.smartfilterbar`
            )->a( n = `xmlns:smartTable`             v = `sap.ui.comp.smarttable`
            )->a( n = `xmlns:smartVariantManagement` v = `sap.ui.comp.smartvariants` ).
```

`smartform`, `smartfield`, `smartchart` and `navpopover` are declared the same way. Since smart controls are metadata-driven, the app usually carries no ABAP data at all — it switches the default model to an OData service instead (see [OData](/cookbook/expert_more/odata)):

```abap
client->view_display( val                       = view->stringify( )
                      switch_default_model_path = `/sap/opu/odata/IWBEP/GWSAMPLE_BASIC/` ).
```

## Page Variant

A page variant is one `SmartVariantManagement` that owns the persistency for the whole page; SmartFilterBar and SmartTable register with it through their `smartvariant` association, each contributing its own `persistencykey`:

```abap
    page->tag( n = `SmartVariantManagement` ns = `smartVariantManagement`
        )->a( n = `id`             v = `pageVariantId`
        )->a( n = `persistencyKey` v = `PageVariantPKey` ).

    page->tag( n = `SmartFilterBar` ns = `smartFilterBar`
        )->a( n = `id`             v = `smartFilterBar`
        )->a( n = `entitySet`      v = `ProductSet`
        )->a( n = `smartVariant`   v = `pageVariantId`
        )->a( n = `persistencyKey` v = `SmartFilterPKey` ).

    page->tag( n = `SmartTable` ns = `smartTable`
        )->a( n = `id`                     v = `smartTable`
        )->a( n = `smartFilterId`          v = `smartFilterBar`
        )->a( n = `smartVariant`           v = `pageVariantId`
        )->a( n = `entitySet`              v = `ProductSet`
        )->a( n = `initiallyVisibleFields` v = `ProductID,Name,Category,Price`
        )->a( n = `useVariantManagement`   v = `true`
        )->a( n = `persistencyKey`         v = `SmartTablePKey` ).
```

Without an annotated `UI.LineItem` the SmartTable starts with no columns at all, so `initiallyVisibleFields` is not optional in practice — name the columns the service is meant to show.

### The `SMART_VARIANT_INIT` Handshake

In a classic UI5 app, the controller calls `initialise( )` on the variant management once the smart controls have registered. Without it, the page variant never gets a personalizable control — saving a view fails in `sap.ui.fl` and stored views are never loaded. In abap2UI5, the `smart_variant_init` frontend event performs this handshake; it waits until the smart controls have registered (which they do once their OData metadata has arrived):

```abap
client->follow_up_action( val   = client->cs_event-smart_variant_init
                          t_arg = VALUE #( ( `pageVariantId` ) ( `smartFilterBar` ) ) ).
```

`t_arg` is positional: the id of the `SmartVariantManagement` and the id of the `SmartFilterBar`.

## Classic FilterBar with Variants

A **classic** `sap.ui.comp.filterbar.FilterBar` knows nothing about variants: every list-report controller hand-writes the same callbacks (`registerFetchData` / `registerApplyData` / `registerGetFiltersWithValues`), adds a `PersonalizableInfo` and marks the variant dirty on each filter change. That is boilerplate over the bar's own filter items — data, not app logic — so the framework owns it. The `filter_bar_variant_init` frontend event wires a classic FilterBar to a `SmartVariantManagement` with no JavaScript in the app:

```abap
client->follow_up_action( val   = client->cs_event-filter_bar_variant_init
                          t_arg = VALUE #( ( `variantId` ) ( `filterBarId` ) ) ).
```

`t_arg` is positional: the id of the `SmartVariantManagement` and the id of the `FilterBar`.

::: tip Samples
The Smart Controls package of the [samples-stack repository](https://github.com/abap2UI5/samples-stack) covers SmartField, SmartForm, SmartTable, the page variant handshake and SmartChart — every class is listed under *Working Samples* at the end of this page.
:::

<!-- samples:start (generated by scripts/link-samples.mjs — do not edit) -->

## Working Samples

Complete apps from the [sample catalog](https://abap2ui5.github.io/playground/samples/)
that use what this page describes. Each is a single class in [abap2UI5/samples](https://github.com/abap2UI5/samples)
unless its row names another of the three sample repositories — pull that repository with
[abapGit](https://abapgit.org) and start the class with `?app_start=<class>`.

| Sample | Class |
|---|---|
| Smart Table and Variants — with variant management - UI_PRODUCTLIST | [`Z2UI5_CL_SMPS_APP_313`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_313.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |
| Smart Multi Input — UI conditions mapped 1:1 onto an ABAP range table | [`Z2UI5_CL_SMPS_APP_319`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_319.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |
| SmartField in a SmartForm — needs the GWSAMPLE_BASIC OData service | [`Z2UI5_CL_SMPS_APP_475`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_475.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |
| SmartForm, editable toggle — needs the GWSAMPLE_BASIC OData service | [`Z2UI5_CL_SMPS_APP_476`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_476.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |
| SmartFilterBar and SmartTable — needs the GWSAMPLE_BASIC OData service | [`Z2UI5_CL_SMPS_APP_477`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_477.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |
| Page Variant Management — needs the GWSAMPLE_BASIC OData service | [`Z2UI5_CL_SMPS_APP_478`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_478.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |
| SmartChart with NavPopover — an analytical service - you supply the path | [`Z2UI5_CL_SMPS_APP_479`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_479.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |
| classic FilterBar variants — no service needed - the data is ABAP | [`Z2UI5_CL_SMPS_APP_493`](https://github.com/abap2UI5/samples-stack/blob/main/src/02/z2ui5_cl_smps_app_493.clas.abap) · [samples-stack](https://github.com/abap2UI5/samples-stack), branch `02-smart-controls` |

<!-- samples:end -->
