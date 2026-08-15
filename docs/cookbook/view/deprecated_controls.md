---
outline: [2, 4]
---
# Deprecated Controls

abap2UI5 sends plain UI5 XML to the browser, so **every** control the UI5 runtime knows will render — including the ones SAP has deprecated. Nothing in the framework stops you from using them.

That makes deprecation a question you have to answer yourself while building the view. A deprecated control still works today, but it receives no new features, may behave inconsistently with newer themes, and disappears without replacement once SAP removes it — which has already happened (the Belize themes were removed in 1.136).

The authoritative, always-current list is the **[UI5 deprecation index](https://ui5.sap.com/#/api/deprecated)**. Check it whenever you pick a control you have not used before. The sections below collect the cases that come up most often in abap2UI5 apps.

## Whole Libraries to Avoid

Do not use *any* control from these libraries — they are deprecated in their entirety:

| Library | Deprecated since | Use instead |
|---|---|---|
| `sap.ui.commons.*` — Accordion, Button, CheckBox, ComboBox, DatePicker, Dialog, FileUploader, Label, Link, Menu, Panel, RadioButton, SearchField, Slider, TextArea, TextField, TextView, ToggleButton, Toolbar, Tree, Form, SimpleForm, AbsoluteLayout, BorderLayout, MatrixLayout, HorizontalLayout, VerticalLayout, … (entire library) | 1.38 | `sap.m` + `sap.ui.layout` |
| `sap.viz.ui5.*` legacy charts — Bar, Bubble, Bullet, Column, Combination, Donut, Heatmap, Line, Pie, Scatter, StackedColumn, Treemap, Waterfall, … | 1.32 | `sap.viz.ui5.controls.VizFrame` |

`sap.ui.commons` is the one to watch out for: several control names exist in both `sap.ui.commons` and `sap.m` (`Button`, `Label`, `Dialog`, `Panel`, …). Copying XML from an old tutorial or an older SAP sample often drags the deprecated namespace along with it.

## Individual Deprecated Controls

| Control | Deprecated since | Use instead |
|---|---|---|
| `sap.m.MultiEditField` | 1.120 | — |
| `sap.f.Avatar` | 1.73 | `sap.m.Avatar` |
| `sap.ui.core.XMLComposite` | 1.88 | Custom controls |
| `sap.ui.core.mvc.HTMLView` | 1.108 | `XMLView` |
| `sap.ui.core.mvc.JSONView` | 1.120 | `XMLView` |
| `sap.ui.core.mvc.JSView` | 1.90 | Typed views |
| `sap.ui.core.mvc.TemplateView` | 1.56 | `XMLView` |
| `sap.ui.core.tmpl.TemplateControl` | 1.56 | — |
| `sap.ui.table.ColumnHeader` | 1.120 | `sap.ui.table.Column` |
| `sap.ui.table.TableHelper` | 1.118 | — |
| `sap.f.routing.Router` / `Target` / `TargetHandler` / `Targets` | 1.56 | `sap.m.routing.*` (async) |
| `sap.tnt.IToolHeader` (interface) | 1.135 | Any control as `ToolPage` header |

## Deprecated Enums and Types

These show up as *property values* rather than as controls, which makes them easy to miss — the view renders, the value is simply ignored or falls back to a default:

- `sap.m.ValueCSSColor`, `DateTimeInputType` (use `DatePicker` / `TimePicker`), `ListHeaderDesign`, `ListMode.SingleSelect` (1.143 → `SingleSelectLeft`), `FrameType.TwoThirds` / `Auto`, the misspelled `PlacementType.*Prefered*` variants
- `sap.f.AvatarShape` / `AvatarSize` / `AvatarType` / `AvatarColor` / `AvatarImageFitType` / `IllustratedMessageType` / `IllustratedMessageSize` / `DynamicPageTitleArea` — use the `sap.m.*` equivalents
- `sap.ui.layout.BlockBackgroundType.Mixed`, `form.GridElementCells`, `SimpleFormLayout.ResponsiveLayout`, `SimpleFormLayout.GridLayout`, `cssgrid.CSSGridGapShortHand`, `GridHelper`
- `sap.ui.table.NavigationMode`, `SortOrder` (use `sap.ui.core.SortOrder`), `VisibleRowCountMode` (use the `rowMode` aggregation), `TreeAutoExpandMode`, `ResetAllMode`
- `sap.ui.core.MessageType` (use `module:sap/ui/core/message/MessageType`)
- `sap.ui.unified.ContentSwitcherAnimation` (1.147 — concept discarded)

## Other Deprecated Items

- Analysis Path Framework (APF) — deprecated 1.140
- `sap.m.PDFViewer.sourceValidationFailed()` — deprecated 1.141
- The declarative `data-sap-ui-type` attribute — deprecated 1.120, use XML views
- Belize, Blue Crystal, and Blue Crystal HCB themes — **removed** in 1.136, use Horizon → [Theme](/configuration/setup/theme)

::: warning Avatar — mind the namespace
Write `Avatar` with no `ns`, so the element resolves to `sap.m.Avatar` through
the view's default `xmlns`:

```abap
)->tag( `Avatar`
    )->a( n = `src` v = `sap-icon://person-placeholder`   " → <Avatar> = sap.m.Avatar
```

**Never write `ns = `f``** — that produces `<f:Avatar>`, the deprecated
`sap.f.Avatar`.

`AvatarGroup` and `AvatarGroupItem` are the other way round: those controls do
still live in `sap.f`, so they need `ns = `f`` and the `sap.f` namespace
declared on the view.
:::


#### Next Steps

- [Definition](/cookbook/view/definition) — how the XML view is built and where to look controls up
- [Cheat Sheet](/cookbook/cheat_sheet) — the rules that matter most, on one page
