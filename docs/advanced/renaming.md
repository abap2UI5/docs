---
outline: [2, 3]
---
# Renaming

The entire abap2UI5 project lives under the `z2ui5` namespace. You may need to rename it to fit customer-specific requirements, like:
- Different modules, e.g., `z2ui5_sd`, `z2ui5_mm`
- Custom namespaces, e.g., `/ZZZ/`, `/YYY/`
- Release-specific naming, e.g., `z2411`, `z2502`

abap2UI5 works with the abaplint renaming feature and supports namespaces up to 9 characters, e.g., `zabap2ui5`.

## Functionality
For more on renaming ABAP artifacts, see:
[Automagic standalone renaming of ABAP objects](https://community.sap.com/t5/application-development-blog-posts/automagic-standalone-renaming-of-abap-objects/ba-p/13499851)

For more background, see the blog post:
[Renaming of ABAP Artifacts - The Power of abaplint and abapGit in ABAP Development](https://www.linkedin.com/pulse/renaming-abap-artifacts-power-abaplint-github-actions-development-kqede/).
