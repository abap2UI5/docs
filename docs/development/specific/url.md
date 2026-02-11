# URL

Working with URLs is a common requirement -- reading parameters from the current URL, opening links in new tabs, or managing browser history.

### Read URL Parameters

Access query parameters from the current URL using the config object:
```abap
DATA(lv_search) = client->get( )-s_config-search.
```

### Open a New Tab
Open a URL in a new browser tab using a frontend event:
```abap
DATA(lv_url) = `https://www.abap2UI5.org`.
client->follow_up_action( client->_event_client(
    val = client->cs_event-open_new_tab
    t_arg = VALUE #( ( lv_url ) ) ) ).
```

### Browser History
For browser history manipulation, check out sample `Z2UI5_CL_DEMO_APP_139`.
