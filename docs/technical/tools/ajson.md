---
outline: [2, 4]
---
# ajson

abap2UI5 handles all frontend-backend communication with JSON. Initially, the framework used SAP's `/UI2/CL_JSON`, but that approach turned out to be problematic: key behavior varied across releases, documentation was sparse, and compatibility issues came up often.

The solution: [ajson](https://github.com/sbcgua/ajson), a solid open-source JSON library that greatly improved abap2UI5's functionality.

**Why ajson?**
- **Zero SAP API Dependencies** — No reliance on release-specific SAP classes
- **Broad Compatibility** — Works smoothly from NW 7.02 to ABAP Cloud
- **Developer-Friendly** — Clear API with thorough documentation
- **Active Maintenance** — Responsive issue resolution and ongoing improvement

#### Integration

ajson plugs directly into the `z2ui5` namespace and installs with abap2UI5 automatically. Use it anywhere in your code:
```abap
DATA(input) = z2ui5_cl_ajson=>parse( json_string ).

DATA(output) = z2ui5_cl_ajson=>create_empty( )->set(
  iv_path = '/name'
  iv_val  = 'value'
)->stringify( ).
```

#### Automatic Updates

Every ajson update and bug fix flows into abap2UI5 automatically through GitHub Actions and the [mirror-ajson](https://github.com/abap2UI5/mirror-ajson) repository. You always run the latest stable version — no manual steps needed.
