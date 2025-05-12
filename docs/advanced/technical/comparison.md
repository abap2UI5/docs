# Comparison to LiveView, Livewire & htmx

**abap2UI5** follows the same fundamental principles as modern "HTML Over the Wire" frameworks like **Phoenix LiveView**, **Laravel Livewire**, and **htmx**.

All of these frameworks simplify web development by rendering the UI on the server and sending it directly to the browser — reducing frontend complexity and keeping logic and state in the backend.

abap2UI5 adapts this idea to the SAP world, allowing developers to build UI5 applications purely in ABAP, without OData, CDS, or complex frontend stacks.

---

## Common Principles

| Concept                   | LiveView / Livewire / htmx                             | abap2UI5                                                       |
|---------------------------|------------------------------------------------------|----------------------------------------------------------------|
| **Server-rendered UI**     | HTML is generated on the server and sent to the client | XML-Views (UI5) are created in ABAP and rendered as HTML in the browser |
| **"Dumb" frontend**        | Browser only displays HTML, no application logic      | UI5 frontend does not know about views or application logic — everything stays in ABAP |
| **Interactions trigger server calls** | Events send AJAX/WebSocket requests to the server     | UI5 events are sent back to ABAP via AJAX (Over the Wire concept) |
| **State remains in the backend** | Application state is managed on the server (LiveView uses WebSocket stateful sessions, Livewire does stateless diffs) | Complete application state is kept in ABAP — frontend holds no state |
| **Delta Rendering**        | Only HTML diffs are sent to update parts of the page  | abap2UI5 sends View + Data together as JSON, frontend updates only changed UI5 controls |
| **No dedicated APIs required** | No OData/REST APIs needed — server returns ready-to-render UI | No OData, CDS, or SEGW — View & Model are sent together in a JSON package |
| **Minimal frontend code**  | Little to no custom JavaScript needed                | Generic UI5 frontend app — all app logic is implemented in ABAP |
| **Backend-driven flexibility** | UI & logic changes are done in backend, no frontend builds | Views, models, and logic are fully controlled by ABAP, no redeployment of frontend apps |

---

## How abap2UI5 differs

While abap2UI5 shares the same architectural ideas, it adapts them to the specifics of SAP environments:

- **View + Data combined as UI5 XML Views & JSON**, not plain HTML fragments like htmx.
- **Stateless communication via AJAX**, similar to Livewire, while offering a stateful-like experience using drafts.
- **UI5 Controls are ABAP objects**, providing code completion & consistency within ADT.
- Designed for SAP systems — leveraging **RTTI**, internal tables, and classic ABAP patterns (e.g., ALV-like apps).
- Avoids OData, RAP & CDS for faster development cycles and reduced complexity.

In short, abap2UI5 brings the simplicity of "HTML Over the Wire" to ABAP developers by enabling full-stack UI5 development with pure ABAP — no JavaScript skills required, no extra frontend layers needed.

---

## Further Reading

- [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html)
- [Laravel Livewire](https://laravel-livewire.com/)
- [htmx](https://htmx.org/)
- [abap2UI5 on GitHub](https://github.com/abap2UI5/abap2UI5)
