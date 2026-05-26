---
outline: [2, 4]
---
# Style / CSS

To set custom styles or CSS:
```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-styles_css = `<<own css>>`.

ENDMETHOD.
```
