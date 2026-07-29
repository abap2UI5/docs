---
outline: [2, 4]
---
# Smart Controls

Smart controls from the `sap.ui.comp` library (SmartFilterBar, SmartTable, SmartForm, SmartField, SmartChart) build their UI from **OData V2 metadata** — combined with variant management, they give you a full list report without hand-built columns or filters.

::: warning SAPUI5 only
The `sap.ui.comp` library ships with SAPUI5 but not with OpenUI5 — apps using smart controls require a SAPUI5 bootstrap. See [UI5 Bootstrapping](/configuration/setup/ui5_bootstrapping).
:::

## Supported Controls

The XML view builder covers the `sap.ui.comp` namespaces `smartfilterbar`, `smarttable`, `smartvariants`, `smartform`, `smartfield`, `smartchart` and `navpopover`. Since smart controls are metadata-driven, the app usually carries no ABAP data at all — it switches the default model to an OData service instead (see [OData](/cookbook/expert_more/odata)):

```abap
client->view_display( val                       = view->stringify( )
                      switch_default_model_path = `/sap/opu/odata/IWBEP/GWSAMPLE_BASIC/` ).
```

## Page Variant

A page variant is one `SmartVariantManagement` that owns the persistency for the whole page; SmartFilterBar and SmartTable register with it through their `smartvariant` association, each contributing its own `persistencykey`:

```abap
page->smart_variant_management(
    id             = `pageVariantId`
    persistencykey = `PageVariantPKey` ).

page->smart_filter_bar(
    id             = `smartFilterBar`
    entityset      = `ProductSet`
    smartvariant   = `pageVariantId`
    persistencykey = `SmartFilterPKey` ).

page->smart_table(
    id                     = `smartTable`
    smartfilterid          = `smartFilterBar`
    smartvariant           = `pageVariantId`
    entityset              = `ProductSet`
    initiallyvisiblefields = `ProductID,Name,Category,Price`
    usevariantmanagement   = `true`
    persistencykey         = `SmartTablePKey` ).
```

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
Demo apps **475–479** in the [samples repository](https://github.com/abap2UI5/samples) cover SmartField, SmartForm, SmartTable, the page variant handshake and SmartChart.
:::
