# abap2UI5 Compared to UI5, RAP & Over-the-Wire Frameworks

This page compares **abap2UI5** with modern "HTML Over the Wire" frameworks like **htmx**, **Phoenix LiveView**, and **Laravel Livewire**.

The goal is to show how abap2UI5 combines Over-the-Wire simplicity with SAP-specific technologies, offering a lightweight alternative for UI5 development.

## Comparison: abap2UI5 vs htmx / LiveView / Livewire

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


## Conclusion

abap2UI5 takes the best of the "HTML Over the Wire" philosophy and applies it to SAP UI5 development. It reduces the overhead of typical SAP frontend architectures by:
- Keeping all logic and state in ABAP.
- Avoiding OData, CDS, and separate frontend deployments.
- Leveraging UI5 XML Views for rendering without losing UI5 flexibility.

For developers familiar with SAP GUI and ALV, abap2UI5 offers a modern yet minimalistic approach to build UI5 applications — fast, backend-driven, and efficient.

## Further Reading

- [abap2UI5 GitHub Repository](https://github.com/abap2UI5/abap2UI5)
- [htmx Documentation](https://htmx.org/docs/)
- [Phoenix LiveView Documentation](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html)
- [Laravel Livewire Documentation](https://laravel-livewire.com/docs)
