---
outline: [2, 4]
---
# Custom Controls (Obsolete)

::: warning Mostly Not Needed Anymore
Earlier versions of abap2UI5 required custom UI5 controls to cover common browser interactions — setting the page title, moving focus, scrolling, copying to the clipboard, opening new tabs, and so on. These are now built into the framework as **frontend events**, callable from ABAP via `client->action( client->cs_event-... )`. The custom controls that used to wrap these behaviors are obsolete.

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
| Start a timer              | `cs_event-start_timer`       | [Timer](/cookbook/browser_interaction/timer)                               |
| Toggle the soft keyboard   | `cs_event-keyboard_set_mode` | [Soft Keyboard](/cookbook/device_capabilities/soft_keyboard)               |

## Typical Pattern

Instead of writing a custom control, call the matching action after an event:

```abap
client->action( val   = client->cs_event-set_title
                t_arg = VALUE #( ( `Invoice 4711` ) ) ).
```

See the linked cookbook pages above for the full argument list of each action.
