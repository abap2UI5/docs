---
outline: [2, 4]
description: abap2UI5 apps in the task pane of Microsoft Excel with the abap2UI5 add-in for Excel - a host page on your SAP system, the add-in manifest, the deployment, Excel on the web, and the ExcelBridge control that lets the app read and write the workbook.
---
# Microsoft Excel

An abap2UI5 app can run in the task pane of Excel, the panel on the right of
the workbook, and it can read and write that workbook. The app stays an ABAP
class: it puts an ABAP table into a sheet, or takes the cells the user has
selected back into the app, with the custom control `z2ui5.cc.ExcelBridge`.
The add-in around it is [`abap2UI5-addons/office-addin`](https://github.com/abap2UI5-addons/office-addin):
a host page you pull into your system with abapGit, and a manifest that tells
Excel where it is.

::: info Preview — proof of concept
The add-in is checked by CI against a stand-in for Office.js, and nobody has
run it in a real Excel yet. `z2ui5.cc.ExcelBridge` ships with abap2UI5
1.147.0 and later.
:::

## How It Works

```
Excel ── task pane
          │  https://<sap-host>/sap/bc/ui5_ui5/sap/z2ui5_xl/index.html?app=Z2UI5_CL_XL_DEMO
          ▼
        host page (BSP Z2UI5_XL)
          ├─ Office.js              from Microsoft's CDN
          ├─ UI5                    the system's own
          └─ z2ui5.embed.Container  the abap2UI5 app, embedded
                │  GET  /sap/bc/z2ui5?z2ui5-bundle   the abap2UI5 frontend
                │  POST /sap/bc/z2ui5                the roundtrips
                ▼
              your app ── z2ui5.cc.ExcelBridge ── the workbook
```

Office.js only works in the top page of the task pane, and abap2UI5's own
page neither loads foreign scripts nor lets itself be framed. So the add-in
brings a page of its own: it loads Office.js and embeds the abap2UI5 app with
the [embed control](/advanced/fiori), the same one a Fiori elements page
uses. The app then runs in the same window as Office.js, which is what lets
`z2ui5.cc.ExcelBridge` reach the workbook.

The host page and the abap2UI5 service share one origin, because abap2UI5
refuses a request from another one. That is why the page is a BSP of the same
system - or, where there are no BSPs, served by an approuter or a reverse
proxy that puts both under one host.

## Requirements

- **abap2UI5 with `z2ui5.cc.ExcelBridge`** - see the note above. The embed
  control itself needs 1.145.0 or later.
- **UI5 1.71 or later** on the system. The host page asks for the theme
  `sap_horizon`, which needs UI5 1.102; below that, write the manifest with
  `--theme sap_fiori_3`.
- **HTTPS** with a certificate the users' machines trust - Office loads
  add-ins over HTTPS only.
- **Excel**: Microsoft 365, or Excel 2019 or later on Windows or Mac. Excel
  on the web works on a best-effort basis, [below](#excel-on-the-web).

## Install

1. **Pull [`abap2UI5-addons/office-addin`](https://github.com/abap2UI5-addons/office-addin)
   with abapGit** into a package of its own, on the system that runs
   abap2UI5. It brings the demo app `Z2UI5_CL_XL_DEMO`, the BSP application
   `Z2UI5_XL` with the host page, and its ICF nodes.
2. **In SICF**, check that `/sap/bc/ui5_ui5/sap/z2ui5_xl` is active, with the
   same logon settings as `/sap/bc/z2ui5`.
3. **Try it in a browser** first:
   `https://<sap-host>/sap/bc/ui5_ui5/sap/z2ui5_xl/index.html?app=Z2UI5_CL_XL_DEMO`
   shows the demo with a line saying it does not run in Excel. The page works
   outside Office as a plain embedding.

On ABAP Cloud there are no BSP applications: pull only the demo app, and
serve the host page from an approuter, as the repository's
`examples/approuter` shows.

## The Manifest

Excel learns about an add-in from its manifest. The repository writes it for
your system and your app, so nobody edits XML by hand:

```bash
npm ci
npm run manifest -- --host https://my.sap.example \
  --app Z2UI5_CL_MY_APP --client 100 --out manifest.xml
```

| Option | |
|---|---|
| `--host` | The system as the users' browsers reach it - HTTPS, origin only |
| `--app` | The abap2UI5 app the task pane starts |
| `--client`, `--language` | `sap-client` and `sap-language` for the page |
| `--param name=value` | A startup parameter, read in the app with `client->get( )-t_comp_params`; repeatable |
| `--app-domain` | Another origin the task pane may navigate to, such as your identity provider; repeatable |
| `--version` | Raise it with every change to a manifest that is already deployed |

One manifest is one app: each gives an add-in of its own, with its own
button on the Home tab. The repository's README lists every option.

## Deploy

- **For your organization:** the Microsoft 365 admin center, **Settings** ›
  **Integrated apps** › **Upload custom apps**, app type *Office Add-in*,
  then the users or groups who get it. The add-in appears in their Excel
  without any setup on their machines; it can take a few hours.
- **For yourself, to try it:** in Excel on the web, open a workbook, then
  **Home** › **Add-ins** › **More Settings** › **Upload My Add-in**. Excel on
  Windows and on the Mac load it from a shared folder or a local folder; the
  README has both.

Once installed, the **abap2UI5** button on the Home tab opens the task pane.
The logon is whatever the ICF node asks for: on the desktop the task pane is
a top-level window, so single sign-on works as it does in a browser.

## Excel on the Web

Excel on the web shows the task pane in a frame inside an Office page, and
two things that do not matter on the desktop decide there:

- **Framing.** The host page's responses - not abap2UI5's own node - have to
  allow the Office hosts as frame ancestors and must not carry
  `X-Frame-Options: SAMEORIGIN`. Microsoft publishes no fixed list of the
  hosts around the task pane; the README gives the set known to host Excel
  on the web and how to set the header with an ICM rewrite rule. Check the
  frame chain in your tenant with the browser's developer tools.
- **Cookies.** In a frame, the SAP session cookies are third-party cookies:
  they need `SameSite=None; Secure`, and a browser that blocks third-party
  cookies, as Safari does, loses the session.

## Apps That Use the Workbook

Any abap2UI5 app runs in the task pane. To talk to the workbook, the view
carries a `z2ui5.cc.ExcelBridge` (it renders nothing), with an `id` and the
bindings it works with:

| Property | |
|---|---|
| `rows`, `columns` | The ABAP table to write, and which of its fields go into the sheet, in which order, under which header |
| `target` | Where: a cell address such as `B3`, `selection`, or `newSheet` with `sheetName` |
| `asTable` | An Excel table with a header row (the default) |
| `selection`, `selectionAddress` | What a read puts back: the selected cells as rows `COL1`, `COL2`, … and their address |
| `available` | Turns true once Excel answered - bind a button's `visible` to it |

The app calls `write` and `read` as a frontend action on that `id`, from a
button without a roundtrip or as a statement after one:

```abap
client->follow_up_action( val   = client->cs_event-control_by_id
                          t_arg = VALUE #( ( `excel` ) ( `write` ) ) ).
```

The control answers with the events `OnWritten`, `OnRead` and `OnError`.
Values keep their types: a string is written as text, so a material number
keeps its leading zeros and nothing becomes a formula; numbers stay numbers
and ABAP dates become Excel dates. A call is capped at 20,000 cells by
default. Outside Excel, in a plain browser, the control fires `OnError`, so
the same app runs in both places.

The demo app `Z2UI5_CL_XL_DEMO` in the repository is the worked example: a
material list with *Export to Excel*, *Take selection* and a *Follow
selection* switch.

## Word, PowerPoint, Outlook

The add-in is built for Excel, and `z2ui5.cc.ExcelBridge` speaks to Excel
only. The approach itself - a host page with Office.js and the embed control
- would carry an abap2UI5 app into the task pane of Word, PowerPoint or
Outlook as well, with a manifest for that host and without access to the
document. None of these exists yet.
