---
outline: [2, 4]
---
# Translation, i18n

In UI5 development, translations are typically managed through i18n files, with translation content stored in frontend artifacts. In abap2UI5, however, since all apps reside on the ABAP backend, we can leverage ABAP's built-in translation mechanisms, such as text elements or message classes.

### Text Elements
The message can be translated using the ABAP text element mechanism, making it available in different languages without changing the code:
```abap
METHOD z2ui5_if_app~main.

  data(lv_msg_translated) = 'this is a translatable message in english'(001).
  client->message_box_display( lv_msg_translated ).

ENDMETHOD.
```

### Messages
Messages are translated using message classes, which ensures that translations are managed centrally and can be maintained easily in different languages:
```abap
METHOD z2ui5_if_app~main.

  MESSAGE ID 'NET' TYPE 'I' NUMBER '001' into data(lv_msg_translated).
  client->message_box_display( lv_msg_translated ). 

ENDMETHOD.
```