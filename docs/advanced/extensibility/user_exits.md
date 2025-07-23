abap2UI5 contains predefined predefined user exits which can be used to modify the standard behaviour. The user exits are exposed by the interface `Z2UI5_IF_EXIT`. To use them in your system you have to create a new class which implements the interface and its methods. They're called dynamically by abap2UI5 class `Z2UI5_CL_EXIT`. You should **not** include your class into abap2UI5 packages but in any other custom package.

These are the available methods:
- `GET_DRAFT_EXP_TIME_IN_HOURS`: You can override the default 4 hour abap2UI5 draft session timeout
- `ADJUST_CONFIG`: You can change abap2UI5 startup parameters like the theme or UI5 version