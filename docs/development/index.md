---
outline: [2, 3]
---
# Cookbook

Practical patterns for everyday abap2UI5 development. The pages in this section build on each other, but you can also dip in for a specific topic — each page is self-contained.

Brand new? Start with the **[Quickstart](/get_started/quickstart)** and **[Hello World](/get_started/hello_world)** first. They install the framework and walk through a minimal app.

## Core

The patterns every app touches.

- **[Controller](/development/general)** — the single-class structure used by most apps: `main`, dispatch via `CASE abap_true`, init/event/navigated phases.
- **[View](/development/view)** — build the XML view via the `z2ui5_cl_xml_view` builder.
- **[Model](/development/model/model)** — bind ABAP data to controls.
- **[Events](/development/events)** — react to button presses, input changes, and other user actions.
- **[Navigation](/development/navigation/navigation)** — chain apps and pass data between them.

## Data Patterns

How to surface and shape data.

- **[Tables, Trees](/development/model/tables)** — bind structured data into table-like controls.
- **[Device Model](/development/model/device)** — query device characteristics (form factor, OS).
- **[Expression Binding](/development/model/expression_binding)** — small in-view formulas without a formatter.
- **[OData](/development/model/odata)** — when you need a real OData model behind a control.

## Interactions

User-facing patterns layered on top of the core flow.

- **[Messages, Errors](/development/messages)** — toasts, message boxes, and error dialogs.
- **[Translation, i18n](/development/translation)** — i18n for view labels and runtime strings.
- **[Popups, Popovers](/development/popups)** — modal and inline prompts.
- **[App State](/development/navigation/app_state)** — preserve state across navigations.
- **[Share, Bookmark](/development/navigation/share)** — make app states deep-linkable.

## Specifics

Domain features and integrations. Each page covers one specific capability.

- **[Barcode Scanning](/development/specific/barcodes)**, **[Camera](/development/specific/camera)**, **[Geolocation](/development/specific/geolocation)** — device hardware.
- **[File Handling](/development/specific/files)**, **[XLSX](/development/specific/xlsx)** — file I/O.
- **[Drag & Drop](/development/specific/drag)**, **[Smart Controls](/development/specific/smart_controls)** — richer UI.
- **[CDS, EML](/development/specific/cds)**, **[OData](/development/model/odata)** — backend data sources.
- **[Logging](/development/specific/logging)**, **[Timer](/development/specific/timer)**, **[CL_DEMO_OUTPUT](/development/specific/demo_output)** — runtime helpers.
- **[URL](/development/specific/url)**, **[Formatter](/development/specific/formatter)** — view-level utilities.

## Stuck?

- **[Troubleshooting](/development/trouble)** — common development-time problems.
- **[Sample Apps](/get_started/samples)** and the **[Samples Index](/resources/samples_index)** — 250+ runnable examples.
- **[FAQ](/resources/faq)** — quick answers to recurring questions.
- **[GitHub Issues](https://github.com/abap2UI5/abap2UI5/issues)** — bugs, feature requests, support.
