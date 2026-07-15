---
outline: [2, 4]
---
# Follow-up Action

Sometimes, once your backend event handler has finished, you want to trigger an
action that runs on the frontend — set the browser title, move focus, scroll,
copy to the clipboard, and so on. `client->follow_up_action( )` schedules such a
frontend action; it runs in the browser right after the response arrives.

```abap
METHOD z2ui5_if_app~main.

    client->follow_up_action( |.eF('OPEN_NEW_TAB', 'https://github.com/abap2UI5')| ).

ENDMETHOD.
```

The string you pass is the frontend call executed after the roundtrip.
`.eF('<EVENT>'<, args>)` invokes one of the built-in frontend events (see
[Frontend](/cookbook/event_navigation/frontend)) — for example `SET_TITLE`,
`SET_FOCUS`, `SCROLL_TO`, `START_TIMER`, `DOWNLOAD_B64_FILE`, `PLAY_AUDIO`. Each
argument is passed as a quoted string:

```abap
client->follow_up_action( |.eF('SET_TITLE', 'Invoice 4711')| ).
```

See the dedicated cookbook pages under [Browser Interaction](/cookbook/browser_interaction/title)
and [Device Capabilities](/cookbook/device_capabilities/upload_download) for the
argument list of each event, and sample `Z2UI5_CL_DEMO_APP_180` for a complete
example.
