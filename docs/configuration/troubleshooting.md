---
outline: [2, 4]
---
# Troubleshooting
Since all logic runs in ABAP, you can debug everything in the ABAP environment. Set an external breakpoint, because HTTP calls abap2UI5 apps externally.

### Backend
Set a breakpoint in your abap2UI5 app to debug the code. Verify that the XML view generates correctly and that all events fire on the backend as expected.

### Frontend
On the frontend, abap2UI5 behaves like a standard UI5 app, so the usual tools and debugging features apply.

#### Debugging Tools
Press `Ctrl+F12` to open the built-in debugging tools in abap2UI5:
![Built-in debugger showing XML View and Data Model inspection](/configuration/debug.png)
From here you can inspect the XML View and check the Data Model bound to the view.

#### UI5 Inspector
Another option: SAP's default debugging tool, the [UI5 Inspector](https://chromewebstore.google.com/detail/ui5-inspector/bebecogbafbighhaildooiibipcnbngo).

### Issues
If your code looks correct but you suspect a bug in the abap2UI5 framework, create a minimal sample that reproduces the bug and open an issue in the abap2UI5 repository — we'll investigate and fix it.
