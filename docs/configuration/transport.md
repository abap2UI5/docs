---
outline: [2, 4]
---
# Transport

Transport abap2UI5 to production like any other ABAP project.

#### Preparations
For a smooth rollout, follow these steps:
1. Transport the abap2UI5 HTTP service and the framework first.
2. Activate the HTTP service explicitly if needed, and adjust the [UI5 bootstrap source](/configuration/setup/ui5_bootstrapping) if production should use a different UI5 version or delivery channel.
3. Test the "Hello World" app to confirm abap2UI5 works as expected.
4. Finally, transport your own apps.

#### Transport
Install the project via abapGit on your development system. Then use the standard transport process to deploy to production:
![Transport process from development to production via abapGit](/configuration/image-3.png){ width=80% }
