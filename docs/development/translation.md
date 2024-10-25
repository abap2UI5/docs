---
outline: [2, 4]
---
# Translation, i18n

In UI5 development, translation is handled through i18n files, with translations stored in frontend artifacts. However, in abap2UI5, all apps are stored entirely in the backend ABAP stack, allowing us to utilize standard ABAP translation mechanisms such as text elements or message texts.

### Text Elements
The message can be translated using the ABAP text element mechanism, making it available in different languages without changing the code:
```abap
METHOD z2ui5_if_app~main.

  data(lv_msg_translated) = 'this is a translatable message in english'(001).
  client->message_box_display( lv_msg_translated ).

ENDMETHOD.
```

### Messages
Messages are translated using the ABAP message class, which ensures that translations are managed centrally and can be maintained easily in different languages:
```abap
METHOD z2ui5_if_app~main.

  MESSAGE ID 'NET' TYPE 'I' NUMBER '001' into data(lv_msg_translated).
  client->message_box_display( lv_msg_translated ). 
  
ENDMETHOD.
```