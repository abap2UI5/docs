# abap2UI5 Compared to UI5, RAP & Over-the-Wire Frameworks

This page provides a structured comparison of **abap2UI5** with:
1. Modern "HTML Over the Wire" frameworks like **htmx, LiveView, Livewire**
2. Traditional **UI5 Freestyle** development
3. SAP’s **RAP (Fiori Elements)** approach

The goal is to show how abap2UI5 combines Over-the-Wire simplicity with SAP-specific technologies, offering a lightweight alternative for UI5 development.

## 1. abap2UI5 vs Over-the-Wire Frameworks

| Concept                   | abap2UI5                                               | htmx / LiveView / Livewire                                  |
|---------------------------|--------------------------------------------------------|-------------------------------------------------------------|
| **UI Rendering**           | ABAP generates UI5 XML Views, rendered in browser       | Server renders HTML, sends fragments to browser              |
| **Frontend knowledge**     | Frontend app only renders Views & Data, no app logic    | Browser is "dumb", no application state or logic             |
| **Interactivity**          | UI events trigger AJAX calls to ABAP backend            | Events trigger AJAX/WebSocket calls to backend               |
| **State Management**       | Backend holds full application state, Drafts simulate session | Backend maintains state (LiveView: WebSockets, Livewire: stateless diffs) |
| **Delta Rendering**        | View & Data sent as JSON diff, only updated controls re-rendered | HTML diffs / fragments sent, partial DOM updates             |
| **APIs required?**         | Simple HTTP handler, no OData/CDS                       | No explicit API, server returns ready-to-render UI           |
| **Frontend development effort** | No custom JS needed, pure ABAP                      | Minimal JS, declarative HTML attributes                     |
| **Deployment**             | Pure ABAP code, no frontend build/deploy, via abapGit    | Standard web app deployment, no frontend frameworks needed   |

> **Summary:**  
abap2UI5 adopts the same backend-driven approach as htmx, LiveView, and Livewire, but uses UI5 XML Views and integrates natively into ABAP environments — combining Over-the-Wire simplicity with SAP UI5 technology.


## 2. abap2UI5 vs UI5 Freestyle

| Concept                   | abap2UI5                                               | UI5 Freestyle                                                |
|---------------------------|--------------------------------------------------------|--------------------------------------------------------------|
| **UI Rendering**           | ABAP generates UI5 XML Views, browser renders UI        | UI5 XML Views created & rendered in frontend (WebIDE/BAS)     |
| **Frontend knowledge**     | Frontend does not know application state or flow        | Full application logic in frontend JS Controllers             |
| **Interactivity**          | Events sent to ABAP backend via AJAX                    | Events handled in frontend, backend provides data via OData   |
| **State Management**       | Managed in ABAP, Drafts emulate frontend state          | State managed in frontend models, backend only persists data  |
| **APIs required?**         | Generic HTTP handler, no OData/CDS needed               | Requires OData Service (SEGW / CAP)                           |
| **Flexibility**            | Full runtime control of Views, Models, Logic in ABAP    | High flexibility in frontend, but complex coordination        |
| **Frontend Artifacts**     | No BSP, index.html is stored in ABAP source code        | BSP App or UI5 repository deployment required                 |
| **Development Workflow**   | Pure ABAP, deploy by class activation                   | Separate frontend & backend development & deployment          |
| **Versioning & Deployment**| Single abapGit project, backend-only transports         | Frontend and backend transported separately                   |

> **Summary:**  
Compared to UI5 Freestyle, abap2UI5 eliminates the need for separate frontend apps, OData services, and JavaScript controllers — enabling faster development cycles with a pure ABAP approach while keeping UI5's capabilities.

## 3. abap2UI5 vs RAP (Fiori Elements)

| Concept                   | abap2UI5                                               | RAP (Fiori Elements)                                         |
|---------------------------|--------------------------------------------------------|--------------------------------------------------------------|
| **UI Rendering**           | UI5 XML Views generated dynamically in ABAP             | UI generated from CDS annotations & templates                 |
| **Frontend knowledge**     | Frontend renders server-provided Views & Data, no logic | Frontend uses annotations, limited runtime flexibility        |
| **Interactivity**          | Events trigger AJAX requests to backend                 | Interactions call OData services defined via CDS & RAP classes |
| **State Management**       | ABAP-driven state & drafts simulate statefulness        | RAP framework handles drafts & state abstraction              |
| **APIs required?**         | No OData/CDS needed, simple generic HTTP handler        | Requires typed OData Services, CDS Views                      |
| **Flexibility**            | Full control of View, Model, Logic in ABAP, runtime changes possible | High structure, runtime changes limited by RAP framework       |
| **Frontend Artifacts**     | No separate UI5 deployment, index.html in ABAP code     | Generated UI5 app artifacts, deployment required               |
| **Development Workflow**   | Pure ABAP, deploy via class activation                  | CDS, OData, UI5 artifacts developed & transported separately   |
| **Versioning & Deployment**| Single abapGit project, cloud & on-prem compatible      | Separate transport of backend & frontend artifacts             |

> **Summary:**  
While RAP enforces structured development through annotations, OData, and CDS artifacts, abap2UI5 offers more runtime flexibility and simpler development by avoiding these layers and keeping everything within ABAP.


## Conclusion

abap2UI5 takes the best of the "HTML Over the Wire" philosophy and applies it to SAP UI5 development. It reduces the overhead of typical SAP frontend architectures by:
- Keeping all logic and state in ABAP.
- Avoiding OData, CDS, and separate frontend deployments.
- Leveraging UI5 XML Views for rendering without losing UI5 flexibility.

For developers familiar with SAP GUI and ALV, abap2UI5 offers a modern yet minimalistic approach to build UI5 applications — fast, backend-driven, and efficient.

## Further Reading

If you want to dive deeper into the technologies and concepts mentioned in this comparison, the following resources are recommended:

###### "HTML Over the Wire" Frameworks
- [Phoenix LiveView Documentation](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html)
- [Laravel Livewire Documentation](https://laravel-livewire.com/docs)
- [htmx Documentation](https://htmx.org/docs/)

###### SAP UI5 & RAP
- [SAPUI5 Freestyle App Development Guide](https://sapui5.hana.ondemand.com/)
- [SAP RAP Guide (ABAP RESTful Application Programming Model)](https://help.sap.com/viewer/product/ABAP_RESTFUL_APPLICATION_PROGRAMMING_MODEL/latest/en-US)
- [Fiori Elements Overview](https://experience.sap.com/fiori-design-web/floorplans/)
