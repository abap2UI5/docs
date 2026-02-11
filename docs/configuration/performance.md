---
outline: [2, 4]
---

# Performance

abap2UI5 is fast. Almost all processing happens on the ABAP backend, which is significantly faster than client-side or browser-based processing.

Frontend logic is kept to a minimum: no business logic runs in the browser. Everything is passed directly to the UI5 framework, which focuses solely on UI rendering.

abap2UI5 has been successfully tested with tables containing large numbers of entries and columns. So, you can confidently develop your app — performance shouldn't be a concern.

### Suggestions
Want to optimize your app even more? Here are a few tips:
* Only call the `client->view_display` method when necessary. Instead, prefer using `client->model_update` so the UI5 framework only re-renders the controls that have actually changed
* Prefer using `client->bind` and use `client->bind_edit` only when users need to make changes that are processed in the backend. Otherwise, it leads to unnecessary data transfers
* Declare public attributes in your app class only for variables displayed in the frontend. This helps prevent the framework from accessing unused values
* Follow standard ABAP best practices, such as reducing loops and using sorted tables, as you would in any other ABAP development project

### Performance Issues?
If you encounter performance issues, try creating a sample and submitting a pull request to the samples repository. We'd be happy to analyze it and see if we can make abap2UI5 even faster.
