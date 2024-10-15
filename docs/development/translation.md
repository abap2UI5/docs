# Translation, i18n

In typical UI5 development, translation is handled through i18n files, with translations stored in frontend artifacts. However, in abap2UI5, all apps are stored entirely in the backend ABAP stack, allowing us to utilize standard ABAP translation mechanisms such as text elements or message texts.

### Text Elements
In this example, the message can be translated using the ABAP text element mechanism, making it available in different languages without changing the code:
```abap
CLASS z2ui5_cl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS z2ui5_cl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    data(lv_message) = 'this is a translatable message in english'(001).
    client->message_box_display( lv_message ).
  ENDMETHOD.
ENDCLASS.
```

### Message Texts
In this case, messages are translated using the ABAP message class, which ensures that translations are managed centrally and can be maintained easily in different languages:
```abap
CLASS z2ui5_cl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS z2ui5_cl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_message).
    client->message_box_display( lv_message ).
  ENDMETHOD.
ENDCLASS.
```