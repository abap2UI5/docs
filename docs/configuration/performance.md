---
outline: [2, 4]
---

# Performance

abap2UI5 is fast! Almost everything is processed in the backend, with the ABAP stack performing significantly faster than any client or browser. <br>

Frontend logic is kept to a minimum: no loops are executed, no extra logic is added, and everything is passed directly to the UI5 framework. <br>

abap2UI5 has been successfully tested with tables containing a large number of entries and columns. So, just develop your app confidently — performance should not be an issue.

### Suggestions
If you still want to optimize your app, consider the following points:
* Only call the method `client->view_display` when it is truly necessary, and prefer using `client->model_update`, this way, the UI5 framework only re-renders the controls that have actually changed.
* Prefer `client->bind` and use `client->bind_edit` only when users need to make changes that are processed in the backend, otherwise, it leads to unnecessary data transfer
* Use public attributes in your app class only for variables that are also displayed in the frontend, otherwise, the framework unnecessarily accesses unused values
* Follow the usual ABAP best practices: reduce loops, use sorted tables, etc., just like with ALV in the past

### Performance Issues?
If you encounter performance issues, try creating a sample and submitting a pull request to the samples repository. We’d be happy to analyze and see if we can make abap2UI5 even faster.
