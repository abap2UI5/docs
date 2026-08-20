---
outline: [2, 4]
---
# Local

abap2UI5-local is a special build that bundles all framework classes into a single HTTP handler class. Besides that, you only need to create two additional database tables (`z2ui5_t_99` and `z2ui5_t_98` — separate from a normal installation's, which is what keeps the two independent). This gives you a self-contained copy of abap2UI5 that runs independently of any other abap2UI5 installation on the same system.

For full details, see the repository: [abap2UI5-local](https://github.com/abap2UI5/abap2UI5-local)
