# URL

### Read URL Parameter

You can read URL parameters using the following snippet:
```abap
DATA(lv_search) = client->get( )-s_config-search.
```

### Open a new Tab

```abap
data(lv_url) = `https://www.abap2UI5.org`.
client->follow_up_action( client->_event_client( val = client->cs_event-open_new_tab t_arg = VALUE #( ( lv_url ) ) ) ).
```

### Change History
Check out sample `Z2UI5_CL_DEMO_APP_139`.