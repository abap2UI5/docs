---
outline: [2, 4]
---
# ajson

abap2UI5 handles all frontend-backend communication via JSON. Initially, the framework used SAP's `/UI2/CL_JSON`, but that approach proved problematic: critical behavior varied across releases, documentation was sparse, and compatibility issues surfaced frequently.

The solution: [ajson](https://github.com/sbcgua/ajson), a robust open-source JSON library that markedly improved abap2UI5's functionality.

**Why ajson?**
- **Zero SAP API Dependencies** — Removes reliance on release-specific SAP classes
- **Universal Compatibility** — Works cleanly from NW 7.02 to ABAP Cloud
- **Developer-Friendly** — Intuitive API with comprehensive documentation
- **Active Maintenance** — Responsive issue resolution and continuous improvements

#### Integration

ajson integrates directly into the `z2ui5` namespace and installs automatically with abap2UI5. Access it anywhere in your code:
```abap
DATA(input) = z2ui5_cl_ajson=>parse( json_string ).

DATA(output) = z2ui5_cl_ajson=>create_empty( )->set(
  iv_path = '/name'
  iv_val  = 'value'
)->stringify( ).
```

#### Automatic Updates

Every ajson update and bug fix flows automatically into abap2UI5 via GitHub Actions and the [mirror-ajson](https://github.com/abap2UI5/mirror-ajson) repository. You always run the latest stable version, no manual intervention needed.
