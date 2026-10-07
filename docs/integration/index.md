---
outline: [2, 4]
description: Every place an abap2UI5 app can appear outside its own browser tab - Fiori Launchpad, SAP Build Work Zone, SAP Mobile Start, a native mobile shell, other UI5 apps and web pages, Microsoft Teams, Microsoft Excel, another ABAP system and AI agents.
---
# Integration

An abap2UI5 app is one URL on your ABAP system, and most places that can show
a web page can show it. This page collects them: the SAP entry points your
users already open, the phone, Microsoft 365, other systems and AI agents.
Each page stands on its own; the table says where to start.

## Where an App Can Appear

| Where | How | Page |
|---|---|---|
| Fiori Launchpad on S/4HANA on-premise or private cloud | A tile with a target mapping on the `z2ui5` app | [Fiori Launchpad](/configuration/launchpad) |
| SAP Build Work Zone | A connector app on BTP, forwarding through a destination | [Build Work Zone](/configuration/btp) |
| A Fiori elements object page, your own UI5 app, a UI Integration Card | The `z2ui5.embed.Container` control from npm | [Fiori Elements Integration](/advanced/fiori) |
| Any web page - React, Vue, Angular or plain HTML | The custom element `<abap2ui5-app>` of an alternative frontend built on UI5 Web Components, for the controls of the portable view profile | [`frontend-webcomponent`](https://github.com/abap2UI5/frontend-webcomponent) |
| SAP Mobile Start on iOS and Android | The tiles of your Work Zone site, mirrored | [Mobile Start](/configuration/mobile_start) |
| A native app on iOS and Android | A generic shell around a webview, with a bridge to the scanner | [Native Mobile Shell](/integration/mobile_shell) |
| Microsoft Teams | A personal tab of a Teams app of your own | [Microsoft Teams](/integration/teams) |
| Microsoft Excel | A task pane add-in; the app can read and write the workbook | [Microsoft Excel](/integration/excel) |
| Another ABAP system | A connector that forwards every roundtrip | [RFC Connector](/advanced/rfc), [HTTP Connector](/advanced/http) |
| An AI agent | The app's screen as data, over MCP | [Agent-Operable Apps](/advanced/agents) |

## Component or Frame

The entries above that show an app inside something else fall into two
kinds, and the kind decides what you have to set up on the ABAP side.

**The app runs as a UI5 component.** The Fiori Launchpad, SAP Build Work
Zone, the embed control and the Excel add-in load the abap2UI5 frontend into
a page of their own, next to the rest of it. abap2UI5's own node is never
framed, and nothing about its HTTP handler changes.

**The app runs in a frame.** A Microsoft Teams tab shows a web page in a
frame of its own, on another origin than Teams. abap2UI5 forbids exactly
that by default: every response carries `X-Frame-Options: SAMEORIGIN`, so a
browser refuses to show the app in a frame anyone else put around it. That is
the protection against clickjacking, and it stays on until an installation
says which hosts may frame it - a few lines in a
[user exit](/advanced/extensibility/user_exits), shown on the Teams page.

Excel on the web sits between the two: the add-in's own page is framed by
Office, the app inside it is not. So the frame policy goes on the add-in's
page, and abap2UI5's node keeps its default.

A frame brings a second question with it, the sign-in. The SAP session
cookie inside a frame is a cookie of another site than the one in the
address bar, and browsers treat those more strictly. The pages on
[Microsoft Teams](/integration/teams#sign-in) and
[Microsoft Excel](/integration/excel#excel-on-the-web) say what that means
there.
