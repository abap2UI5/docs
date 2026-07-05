---
outline: [2, 4]
---
# Mobile Start

SAP Mobile Start surfaces your abap2UI5 apps as tiles in SAP's native mobile entry-point app (iOS/Android). No extra abap2UI5 development is needed — Mobile Start mirrors the content of your SAP Build Work Zone site.

### Setup

1. Complete the [BTP Build Work Zone](/configuration/btp) setup: destination to your ABAP system, deployed connector app, and the app added as Work Zone content.
2. Enable **SAP Mobile Start** for your Work Zone site (Site Settings) and connect the Mobile Start app on the device to the site — typically by scanning the QR code from the site's settings page.
3. Every abap2UI5 tile that is assigned to the user's role in Work Zone automatically appears in Mobile Start; tapping it opens the app in the mobile shell, rendered by the same ABAP backend.

Since abap2UI5 views are responsive UI5 controls, most apps work on phones as they are — check layouts with narrow screens in mind (see the [Device Model](/cookbook/model/device_model) page for adapting views to the device).

### Further Reading
The original article with step-by-step screenshots: [Setup SAP Mobile Start](https://www.linkedin.com/pulse/abap2ui5-integration-sap-business-technology-platform-33-setup-uzure/).
