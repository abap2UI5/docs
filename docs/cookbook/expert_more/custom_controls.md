---
outline: [2, 4]
---
# Custom Controls (Obsolete)

::: warning Mostly Not Needed Anymore
Earlier versions of abap2UI5 required custom UI5 controls to cover common browser interactions — setting the page title, moving focus, scrolling, copying to the clipboard, opening new tabs, and so on. These are now built into the framework as **frontend events**, callable from ABAP via `client->action( val = client->cs_event-... )`. The custom controls that used to wrap these behaviors are obsolete.

You can still build your own [Custom Controls](/advanced/extensibility/custom_control) when you need something the framework does not provide, but for everything listed below, use the dedicated built-in action instead.
:::

## Replaced Custom Controls

The table below lists custom controls that are no longer necessary and the built-in action that replaces each one.

| Old Custom Control Purpose | New Built-in Action          | Documentation                                                              |
| -------------------------- | ---------------------------- | -------------------------------------------------------------------------- |
| Set the browser tab title  | `cs_event-set_title`         | [Title](/cookbook/browser_interaction/title)                               |
| Move input focus           | `cs_event-set_focus`         | [Focus](/cookbook/browser_interaction/focus)                               |
| Scroll to a position       | `cs_event-scroll_to`         | [Scrolling](/cookbook/browser_interaction/scrolling)                       |
| Scroll an element into view| `cs_event-scroll_into_view`  | [Scrolling](/cookbook/browser_interaction/scrolling)                       |
| Copy to clipboard          | `cs_event-clipboard_copy`    | [Clipboard](/cookbook/browser_interaction/clipboard)                       |
| Open a URL / new tab       | `cs_event-open_new_tab`      | [URL Handling](/cookbook/browser_interaction/url_handling)                 |
| Start a timer              | `cs_event-start_timer`       | [Timer](/cookbook/browser_interaction/timer)                               |
| Toggle the soft keyboard   | `cs_event-keyboard_set_mode` | [Soft Keyboard](/cookbook/device_capabilities/soft_keyboard)               |
| Trigger a file download    | `cs_event-download_b`        | [Upload, Download](/cookbook/device_capabilities/upload_download)          |
| Play an audio file         | `cs_event-play_audio`        | [Barcode Scanning](/cookbook/device_capabilities/barcode_scanning)         |

## Why Prefer the Built-in Actions

- **No frontend code to maintain.** The action runs framework-side JavaScript that ships with abap2UI5 — you do not write or deploy your own JS.
- **Stays in pure ABAP.** Calling `client->action( )` keeps your app logic in one place and one language.
- **Reviewed and tested.** The built-in actions are part of the framework, covered by its tests, and updated alongside it.
- **Safer.** Custom controls usually involve embedding JavaScript via [Custom JS](/cookbook/expert_more/custom_js), which carries the security risks documented on that page.

## Typical Pattern

Instead of writing a custom control, call the matching action after an event:

```abap
client->action( val   = client->cs_event-set_title
                t_arg = VALUE #( ( `Invoice 4711` ) ) ).
```

See the linked cookbook pages above for the full argument list of each action.

## When Custom Controls Still Make Sense

Building a real [Custom Control](/advanced/extensibility/custom_control) is still appropriate when:

- You need a UI element that UI5 does not ship and no abap2UI5 action covers.
- You are integrating a third-party UI5 library.
- You need behavior beyond simple frontend method calls — e.g. a stateful widget with its own rendering.

In all other cases, prefer the built-in action.
