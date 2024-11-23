---
outline: [2, 4]
---

# Performance

abap2UI5 is fast! Almost everything is processed in the backend, leveraging the ABAP stack, which performs significantly faster than client-side or browser-based processing. <br>

Frontend logic is kept to a minimum: no business logic is executed, and everything is passed directly to the UI5 framework, focusing solely on UI rendering. <br>

abap2UI5 has been successfully tested with tables containing large numbers of entries and columns. So, you can confidently develop your app — performance should not be a concern.

### Suggestions
If you still want to optimize your app, consider the following tips:
* Only call the `client->view_display` method when necessary. Instead, prefer using `client->model_update` so that the UI5 framework only re-renders the controls that have actually changed
* Prefer using `client->bind` and use `client->bind_edit` only when users need to make changes that are processed in the backend. Otherwise, it leads to unnecessary data transfers
* Declare public attributes in your app class only for variables displayed in the frontend. This helps prevent the framework from accessing unused values
* Follow standard ABAP best practices, such as reducing loops and using sorted tables, as you would in any other ABAP development project

### Performance Issues?
If you encounter performance issues, try creating a sample and submitting a pull request to the samples repository. We would be happy to analyze it and see if we can make abap2UI5 even faster.
