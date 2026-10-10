---
outline: [2, 4]
description: The abap2UI5 native mobile shell - a proof of concept that runs abap2UI5 apps in a generic iOS and Android app built for the SAP mobile stack, with a bridge to the camera, and the NativeBridgeScan control that uses it.
---
# Native Mobile Shell

[abap2UI5/mobile-shell](https://github.com/abap2UI5/mobile-shell) runs
abap2UI5 apps in a native app on iOS and Android. The app is a generic shell:
it knows one URL, shows the regular abap2UI5 frontend in a webview, and
offers the page a small bridge to what a browser cannot reach - the native
barcode scanner, push notifications, the biometric unlock. Every screen
still comes from the ABAP class, so a change to an app never needs a new
release in an app store.

::: info Preview — proof of concept
The shell is a proof of concept, not a product: it has no releases, and
parts of it - the onboarding flow of the SAP BTP SDK, push notifications -
need accounts with SAP, Google and Apple to switch on. Its
[PLAN.md](https://github.com/abap2UI5/mobile-shell/blob/main/PLAN.md) has the
state of every phase.
:::

## Where It Fits

There are two ways to bring an abap2UI5 app to a phone:

| | |
|---|---|
| [Mobile Start](/configuration/mobile_start) | SAP's own app mirrors the tiles of your SAP Build Work Zone site. Available today, no development - but the app sees only what a mobile browser sees |
| Native mobile shell | An app of your own, built on the SAP mobile stack (SAP Mobile Services and the SAP BTP SDK for iOS and Android), with native device features through a bridge |

Both are online-only. abap2UI5 sends every event to the server and gets the
next view back, so the offline features of the SAP mobile stack do not
apply; this is a decision, not a gap.

## How It Works

```
device
├── native shell app       onboarding, app lock, push, managed configuration
│   └── webview            the abap2UI5 frontend, loaded from your system
│         └── window.abap2ui5Native   the bridge
└── HTTPS ── SAP Mobile Services ── your ABAP system with abap2UI5
```

The shell injects a small script into the page that defines
`window.abap2ui5Native`: device information, a native toast, the barcode
scanner, the push token and a biometric confirmation, each as a method that
returns a promise. In a plain browser the object does not exist, and an app
falls back to what the browser can do.

## Scanning From ABAP

The framework ships the custom control `z2ui5.cc.NativeBridgeScan` for the
scanner: a button that calls the shell's scanner, writes the result into its
bound `value` and fires `OnScan`; a canceled or failed scan fires
`OnError`. Outside the shell it renders an invisible placeholder, so the same
view runs unchanged in a browser - `showInBrowser` shows the button anyway,
and a press then fires `OnError`. The control ships with abap2UI5 1.147.0
and later.

The sample app in the repository's `abap/src` is the worked example.

## Distribution

The shell reads its settings from the device management your organization
already uses: the abap2UI5 URL, a forced app lock and screenshot protection
come in as managed configuration on Android and iOS, so no user has to type
an address. A device without such management is set up by scanning a QR
code with the URL. The repository's
[distribution guide](https://github.com/abap2UI5/mobile-shell/blob/main/docs/DISTRIBUTION.md)
has the details and the checklist for production.
