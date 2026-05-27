---
outline: [2, 4]
---
# Bootstrapping

To pick the source for bootstrapping UI5:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-src = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js`.

ENDMETHOD.
```
See the bootstrapping variants and SAP limitations, documented in the [SAP UI5 docs](https://sapui5.hana.ondemand.com/#/topic/2d3eb2f322ea4a82983c1c62a33ec4ae).
