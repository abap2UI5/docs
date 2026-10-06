---
outline: [2, 4]
---
# Style / CSS

UI5 supports app-specific CSS in addition to the theme. abap2UI5 injects whatever string you assign to `cs_config-styles_css` directly into a `<style>` block in the page `<head>`, so any selector you write is applied to your application.

```abap
METHOD z2ui5_if_ui5_exit~set_config_http_get.

    cs_config-styles_css =
      |.myRedButton .sapMBtnContent \{ color: red; font-weight: bold; \}|.

ENDMETHOD.
```

In the XML view you then reference your class via the `class` property:

```abap
    )->tag( `Button`
        )->a( n = `text`  v = `Delete`
        )->a( n = `class` v = `myRedButton` )
```

The `class` lands on the control's outermost element, and a rule there loses wherever the theme styles an element inside it. A button's text color is set on its inner element, so `.myRedButton { color: red; }` alone leaves the text as it was; the rule above reaches into the button for that reason - through an internal class, with the caveat the tips below give. The same goes for the page: `body { background-color: … }` loses to the theme's `.sapUiBody` rule, and an `App` paints the theme's background over the body anyway.

## When to Use Custom CSS

- Tweak spacing, colors or fonts that the theme does not expose as a control property.
- Style abap2UI5 features that don't have a built-in option (e.g. a corporate background image).
- Override the SAP control look in edge cases.

For larger visual changes — corporate fonts, brand colors, custom logo — prefer the official [UI Theme Designer](https://sapui5.hana.ondemand.com/#/topic/be8f7c61bb2444299b3f3429b986e8be). It produces a self-contained theme that you can host yourself and assign via `cs_config-theme`. This keeps your styles maintainable across UI5 upgrades.

## Tips

- Be careful with selectors that target UI5 internals (`.sapMBtn`, `.sapUiTableCell`, …). They are not part of UI5's public API and may change between versions.
- Wrap rules in a parent class (e.g. `.myApp .sapMTitle`) to limit their reach.
- Inside ABAP string templates (`| … |`), the curly braces `{` and `}` must be escaped as `\{` and `\}`.

See the [UI5 styling documentation](https://sapui5.hana.ondemand.com/#/topic/9c9e14990d864bb799d70d2bc6c7d4f7) for guidance on what is safe to customize.
