# Performance

abap2UI5 is fast! Nearly everything is done in the backend, and the ABAP stack is much faster than any client or browser. Frontend logic is kept to a minimum: no loops are executed, and no extra logic is added. Everything is prepared in the backend and then directly übergeben into the UI5 framework at the client side. abap2UI5 has been successfully tested with tables containing a large number of entries and columns. So, just develop your app, and you can assume that performance will not be an issue.<br>

### Suggestions
If you still want to optimize your app, consider the following points:
* Only call the method `client->view_display` when it is truly necessary, and prefer using `client->model_update`, this way, the UI5 framework only re-renders the controls that have actually changed.
* Use `client->bind_edit` only when users need to make changes that are processed in the backend, otherwise, it leads to unnecessary data transfer
* Use public attributes in your app class only for variables that are also displayed in the frontend, otherwise, the framework unnecessarily accesses unused values.
* Follow the usual ABAP best practices: reduce loops, use sorted tables, etc., just like with ALV in the past

### Performance Issues?
Try to minimize them and create a sample. We are happy to analyze and check if we can make abap2UI5 run even faster.