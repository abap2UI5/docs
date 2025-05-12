# Comparison to LiveView, Livewire & htmx

**abap2UI5** follows the same fundamental principles as modern "HTML Over the Wire" frameworks like **Phoenix LiveView**, **Laravel Livewire**, and **htmx** — while also contrasting it with **UI5 Freestyle** development and **RAP (Fiori Elements)**.

The goal is to reduce frontend complexity by keeping logic and state in the backend, enabling ABAP developers to build UI5 applications with a clean, backend-driven approach.

---

## Comparison at a Glance

| Concept                   | LiveView / Livewire / htmx                             | abap2UI5                                                       | UI5 Freestyle                                              | RAP (Fiori Elements)                                       |
|---------------------------|------------------------------------------------------|----------------------------------------------------------------|-----------------------------------------------------------|------------------------------------------------------------|
| **Server-rendered UI**     | HTML is generated on the server and sent to the client | XML-Views (UI5) are generated in ABAP, rendered in the browser  | Frontend (XML Views, JS/Controller) renders data from backend | UI defined via annotations & templates, rendered in frontend |
| **Frontend is "dumb"**     | Browser displays HTML, no application state           | UI5 app only renders server-provided view & data               | Full application logic in frontend controller              | Minimal frontend logic, driven by backend annotations       |
| **Interactivity triggers server calls** | AJAX/WebSocket calls on user events                     | Events sent via AJAX to ABAP backend                           | Event handling in frontend, OData used for data exchange    | OData requests on UI interactions                          |
| **State remains in the backend** | State managed server-side                              | State & flow fully in ABAP, drafts simulate persistence        | State often kept in frontend models                        | State persistence managed via RAP framework (drafts etc.)   |
| **Delta Rendering**        | HTML diffs sent to update UI                          | View & Data sent as JSON diff, partial UI5 control updates     | Full re-rendering managed in frontend controllers          | Smart controls manage partial updates based on metadata     |
| **APIs required?**         | No explicit API needed, returns UI                    | No OData/CDS necessary, simple HTTP handler used               | Requires OData Service (SEGW or CAP)                       | Requires typed OData services & CDS artifacts               |
| **Frontend development effort** | Minimal JS, mostly markup-driven                   | No custom JS needed, pure ABAP                                | Separate frontend app (BAS/WebIDE), heavy JS development    | Generated UI, low-code approach, limited flexibility        |
| **Backend-driven flexibility** | Full flexibility, logic stays on server               | Complete control in ABAP classes (Views, Models, Logic)        | Backend delivers data only, UI logic in JS                 | Backend defines UI behavior via annotations, limited runtime flexibility |
| **Custom UI controls**     | Can be added via normal HTML/JS                       | Possible via Over-the-Wire delivery of JS/HTML                 | Developed separately, integrated in frontend               | Limited to RAP-defined extensibility options                |

---

## What makes abap2UI5 different?

While abap2UI5 follows the "Over the Wire" principle like LiveView, Livewire, and htmx, it is adapted to the SAP ecosystem:

- Instead of sending raw HTML, abap2UI5 transmits **UI5 XML Views and Data models** to the frontend.
- Uses **stateless AJAX requests**, simulating stateful behavior via ABAP drafts.
- Avoids OData, CDS & RAP layers — reduces backend artifacts to a minimum.
- Leverages **ABAP Classes** as UI components, offering code completion and consistency.
- Brings back an ALV-like development flow for modern UI5 apps.

Compared to UI5 Freestyle and RAP:
- abap2UI5 offers more flexibility than RAP but avoids the complexity of Freestyle frontend stacks.
- UI logic and state remain entirely in ABAP, without maintaining separate frontend projects.

---

## Conclusion

abap2UI5 combines the simplicity of "HTML Over the Wire" with SAP UI5's capabilities — fully backend-driven, minimalistic, and flexible. It offers ABAP developers a way to build UI5 applications without the overhead of OData, CAP, or JavaScript-heavy frontend apps.

For developers used to SAP GUI or ALV-based programming, abap2UI5 feels familiar while embracing modern web technologies under the hood.

---

## Further Reading

- [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html)
- [Laravel Livewire](https://laravel-livewire.com/)
- [htmx](https://htmx.org/)
- [SAPUI5 Freestyle Documentation](https://sapui5.hana.ondemand.com/)
- [RAP Guide (Fiori Elements)](https://help.sap.com/viewer/product/ABAP_RESTFUL_APPLICATION_PROGRAMMING_MODEL/latest/en-US)
- [abap2UI5 on GitHub](https://github.com/abap2UI5/abap2UI5)
