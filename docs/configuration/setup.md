---
outline: [2, 4]
---
# Setup

You can run abap2UI5 with a variety of custom configurations. The default setup works out of the box. To customize it, implement the `z2ui5_if_exit` interface:

```abap
CLASS zcl_a2ui5_user_exit DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES  z2ui5_if_exit.

ENDCLASS.
```

See the following pages for specific options:
- [Theme](/configuration/setup/theme)
- [UI5 Bootstrapping](/configuration/setup/ui5_bootstrapping)
- [Title](/configuration/setup/title)
- [Style / CSS](/configuration/setup/style_css)
- [Logon Language](/configuration/setup/logon_language)
