---
outline: [2, 4]
---
# File Download, Upload



#### Download

```abap
METHOD z2ui5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->page(
            )->button(
                text = 'Open Download Popup'
                press = client->_event( 'BUTTON_DOWNLOAD' )
        )->stringify( ) ).

    CASE client->get( )-event.
      WHEN 'BUTTON_DOWNLOAD'.
        DATA(lv_name) = `Default_File_Name.jpg`.
        DATA(lv_content) = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAApgAAA` &&
        `KYB3X3/OAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAANCSURBVEiJtZZPbBtFFMZ/M7ubXdtdb1xSFyeilBapySVU8h8OoFaooFSqiihIVIp` &&
        `QBKci6KEg9Q6H9kovIHoCIVQJJCKE1ENFjnAgcaSGC6rEnxBwA04Tx43t2FnvDAfjkNibxgHxnWb2e/u992bee7tCa00YFsffekFY+nUzFtjW0LrvjRXrCDIAaPLlW` &&
        `0nHL0SsZtVoaF98mLrx3pdhOqLtYPHChahZcYYO7KvPFxvRl5XPp1sN3adWiD1ZAqD6XYK1b/dvE5IWryTt2udLFedwc1+9kLp+vbbpoDh+6TklxBeAi9TL0taeWpd` &&
        `mZzQDry0AcO+jQ12RyohqqoYoo8RDwJrU+qXkjWtfi8Xxt58BdQuwQs9qC/afLwCw8tnQbqYAPsgxE1S6F3EAIXux2oQFKm0ihMsOF71dHYx+f3NND68ghCu1YIoeP` &&
        `PQN1pGRABkJ6Bus96CutRZMydTl+TvuiRW1m3n0eDl0vRPcEysqdXn+jsQPsrHMquGeXEaY4Yk4wxWcY5V/9scqOMOVUFthatyTy8QyqwZ+kDURKoMWxNKr2EeqVKc` &&
        `TNOajqKoBgOE28U4tdQl5p5bwCw7BWquaZSzAPlwjlithJtp3pTImSqQRrb2Z8PHGigD4RZuNX6JYj6wj7O4TFLbCO/Mn/m8R+h6rYSUb3ekokRY6f/YukArN979jc` &&
        `W+V/S8g0eT/N3VN3kTqWbQ428m9/8k0P/1aIhF36PccEl6EhOcAUCrXKZXXWS3XKd2vc/TRBG9O5ELC17MmWubD2nKhUKZa26Ba2+D3P+4/MNCFwg59oWVeYhkzgN/` &&
        `JDR8deKBoD7Y+ljEjGZ0sosXVTvbc6RHirr2reNy1OXd6pJsQ+gqjk8VWFYmHrwBzW/n+uMPFiRwHB2I7ih8ciHFxIkd/3Omk5tCDV1t+2nNu5sxxpDFNx+huNhVT3` &&
        `/zMDz8usXC3ddaHBj1GHj/As08fwTS7Kt1HBTmyN29vdwAw+/wbwLVOJ3uAD1wi/dUH7Qei66PfyuRj4Ik9is+hglfbkbfR3cnZm7chlUWLdwmprtCohX4HUtlOcQj` &&
        `LYCu+fzGJH2QRKvP3UNz8bWk1qMxjGTOMThZ3kvgLI5AzFfo379UAAAAASUVORK5CYII=`.

        client->follow_up_action( val = client->_event_client(
            val   = client->cs_event-download_b64_file
            t_arg = VALUE #( ( lv_content ) ( lv_name ) ) ) ).

    ENDCASE.

ENDMETHOD.
```


#### Upload

```abap
CLASS z2ui5_cl_sample_upload DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES Z2UI5_if_app.
    DATA mv_path  TYPE string.
    DATA mv_value TYPE string.

ENDCLASS.

CLASS z2ui5_cl_sample_upload IMPLEMENTATION.
  METHOD Z2UI5_if_app~main.

    client->view_display( z2ui5_cl_xml_view=>factory(
        )->page(
            )->_z2ui5( )->file_uploader(
                value       = client->_bind_edit( mv_value )
                path        = client->_bind_edit( mv_path )
                placeholder = 'filepath here...'
                upload      = client->_event( 'UPLOAD' )
        )->stringify( ) ).

    CASE client->get( )-event.
      WHEN 'UPLOAD'.
        "process with mv_value and mv_path...
        client->message_box_display( `file uploaded` ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
```