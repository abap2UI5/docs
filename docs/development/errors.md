---
outline: [2, 4]
---
# Errors

### Uncatched Errors

#### Assert
The most simple apporach is to just use an assert and output the error directly via the http handler:
```abap
CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    ASSERT 1 = `This is an error message!`.

  ENDMETHOD.
ENDCLASS.
```
####  Exception
You can achieve the same behavior with an uncaught exception. The framework will convert it into an assert and stop execution:
```abap
CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    RAISE EXCEPTION NEW lcx_error( ).

  ENDMETHOD.
ENDCLASS.
```

### Catched Errors

For end users, it’s better to create a UI5 popup that displays the error:

#### Exception with Message Box
```abap
CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.

    TRY.

        "implementation here...
        RAISE EXCEPTION NEW lcx_error( ).

      CATCH cx_root INTO DATA(lx).
        client->message_box_display( |An error occured: { lx->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
```

#### Popup Exception
Or use the built-in error popup to display more details:

```abap
CLASS zcl_my_app IMPLEMENTATION.
  METHOD z2ui5_if_app~main.
    TRY.

        "implementation here...
        RAISE EXCEPTION NEW lcx_error( ).

      CATCH cx_root INTO DATA(lx).
        client->nav_app_call( z2ui5_cl_pop_error=>factory( lx ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```