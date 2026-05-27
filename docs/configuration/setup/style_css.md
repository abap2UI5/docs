---
outline: [2, 4]
---
# Style / CSS

UI5 supports app-specific CSS in addition to the theme. abap2UI5 injects whatever string you assign to `cs_config-styles_css` directly into a `<style>` block in the page `<head>`, so any selector you write is applied to your application.

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-styles_css =
      |body \{ background-color: #f5f5f5; \}| &&
      |.myRedButton \{ color: red; font-weight: bold; \}|.

ENDMETHOD.
```

In the XML view you then reference your class via the `class` property:

```abap
view->button( text = `Delete` class = `myRedButton` ).
```

## When to Use Custom CSS

- Tweak spacing, colours or fonts that the theme does not expose as a control property.
- Style abap2UI5 features that don't have a built-in option (e.g. a corporate background image).
- Override the SAP control look in edge cases.

For larger visual changes — corporate fonts, brand colours, custom logo — prefer the official [UI Theme Designer](https://sapui5.hana.ondemand.com/#/topic/be8f7c61bb2444299b3f3429b986e8be). It produces a self-contained theme that you can host yourself and assign via `cs_config-theme`. This keeps your styles maintainable across UI5 upgrades.

## Tips

- Be careful with selectors that target UI5 internals (`.sapMBtn`, `.sapUiTableCell`, …). They are not part of UI5's public API and may change between versions.
- Wrap rules in a parent class (e.g. `.myApp .sapMTitle`) to limit their reach.
- Inside ABAP string templates (`| … |`), the curly braces `{` and `}` must be escaped as `\{` and `\}`.

See the [UI5 styling documentation](https://sapui5.hana.ondemand.com/#/topic/9c9e14990d864bb799d70d2bc6c7d4f7) for guidance on what is safe to customise.
