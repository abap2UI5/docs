---
outline: [2, 4]
---
# URL Handling

Working with URLs is common — reading parameters from the current URL, opening links in new tabs, or managing browser history.

### Read URL Parameters

Read query parameters from the current URL via the config object:
```abap
DATA(lv_search) = client->get( )-s_config-search.
```

### Open a New Tab
Open a URL in a new browser tab via a frontend event:
```abap
DATA(lv_url) = `https://www.abap2UI5.org`.
client->action->gen(
    val   = client->cs_event-open_new_tab
    t_arg = VALUE #( ( lv_url ) ) ).
```

### Browser History
Two client methods control the browser history from the backend:

Push a new history entry — the value is appended to the URL hash, so app state becomes bookmarkable and the browser back button steps through your pushed states:
```abap
client->set_push_state( `&my-app-state=detail` ).
```

Trigger a browser back navigation (`history.back()`) with the next response:
```abap
client->set_nav_back( ).
```

For a complete example, see sample `Z2UI5_CL_DEMO_APP_139`.
