---
outline: [2, 4]
---
# Favicon

The favicon is the small icon a browser shows in the tab, in the bookmark list and on the history page. In a hand-written `index.html` it is a `<link rel="icon">` pointing at a file. In abap2UI5 you set it from ABAP:

```abap
METHOD z2ui5_if_exit~set_config_http_get.

    cs_config-favicon = `/sap/public/bc/ui2/logo/company.png`.

ENDMETHOD.
```

If you leave the field untouched, abap2UI5 uses its own mark — the same one this documentation and the sample pages show. Clear the field and the page carries no `<link rel="icon">` at all, which lets the browser fall back to `/favicon.ico` on your host:

```abap
CLEAR cs_config-favicon.
```

## What You Can Put In It

Anything a browser accepts as an icon URL:

| Value | Use it when |
|---|---|
| an absolute path on your system (`/sap/public/…`, a MIME repository object, a BSP resource) | the icon is already deployed somewhere on the host |
| a full URL | the icon is served by another host — check your [Content Security Policy](/configuration/security) allows that origin |
| a `data:` URI | you want no deployment step at all; the default CSP already allows `data:` |

The framework's own default is a `data:` URI holding a two-element SVG, because the page is generated in ABAP and has no static resources of its own:

```abap
cs_config-favicon =
  |data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' | &&
  |viewBox='0 0 40 40' fill='%23fff'>| &&
  |<circle cx='20' cy='20' r='20' fill='%23d03c4a'/></svg>|.
```

Two things to know about SVG data URIs: the markup must be **well-formed XML** (a stray unclosed tag gives you no icon and no error), and it may contain no double quotes, since it sits inside one — use single quotes for the attributes and `%23` for `#` in a colour.

## Changing It While the App Runs

`cs_config-favicon` is read once, when the browser asks for the page. To change the icon from inside a running app — a status indicator, a per-app icon in a multi-app system — use the `SET_FAVICON` frontend action instead:

```abap
client->follow_up_action(
    val   = z2ui5_if_client=>cs_event-set_favicon
    t_arg = VALUE #( ( `data:image/svg+xml,<svg …/></svg>` ) ) ).
```

See [Set the Tab Title and Favicon](/cookbook/browser_interaction/title) for the runnable sample.

## See Also

- [Setup](/configuration/setup) — every field on `cs_config` and the page that documents it.
- [Security](/configuration/security) — the Content Security Policy, which decides what an icon URL may point at.
