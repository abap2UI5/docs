---
outline: [2, 4]
---
# Renaming

The entire abap2UI5 project lives under the `z2ui5` namespace. You may need to rename it to fit customer-specific requirements, like:
- Different modules, e.g., `z2ui5_sd`, `z2ui5_mm`
- Custom namespaces, e.g., `/ZZZ/`, `/YYY/`
- Release-specific naming, e.g., `z2411`, `z2502`

abap2UI5 works with the abaplint renaming feature and supports namespaces up to 9 characters, e.g., `zabap2ui5`.

#### How It Works
[abaplint](https://abaplint.org) can rename ABAP artifacts across a whole repository: you define rename patterns (old name → new name, including regular expressions) in an abaplint configuration, and `abaplint --rename` rewrites every class, interface, and reference consistently, writing the result to an output folder:

```jsonc
"rename": {
  "output": "output",
  "patterns": [
    { "type": "CLAS|INTF", "oldName": "z2ui5(.*)", "newName": "zmyns$1" }
  ]
}
```

The renamed copy is a complete, installable abapGit project under your own namespace — install it side by side with the original, pin it to a release, or ship it inside your product. The abap2UI5 CI runs this transformation on every change (`npm run rename`, workflow `test_rename.yaml`) to guarantee the codebase stays renameable.

For a ready-made pipeline that builds a renamed release with your chosen add-ons included, see the [Builder](/advanced/builds).

#### Further Reading
- [Automagic standalone renaming of ABAP objects](https://community.sap.com/t5/application-development-blog-posts/automagic-standalone-renaming-of-abap-objects/ba-p/13499851)
- [Renaming of ABAP Artifacts — The Power of abaplint and abapGit in ABAP Development](https://www.linkedin.com/pulse/renaming-abap-artifacts-power-abaplint-github-actions-development-kqede/)
