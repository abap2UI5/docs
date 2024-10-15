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
// [!code focus]
    data(lv_message) = 'this is a translatable message in english'(001).// [!code focus]
    client->message_box_display( lv_message ).// [!code focus]
// [!code focus]
  ENDMETHOD.
ENDCLASS.
```

### Messages
In this case, messages are translated using the ABAP message class, which ensures that translations are managed centrally and can be maintained easily in different languages:
```abap
CLASS z2ui5_cl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

CLASS z2ui5_cl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
// [!code focus]
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno // [!code focus]
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 // [!code focus]
        INTO DATA(lv_message).// [!code focus]
    client->message_box_display( lv_message ). // [!code focus]
// [!code focus]
  ENDMETHOD.
ENDCLASS.
```