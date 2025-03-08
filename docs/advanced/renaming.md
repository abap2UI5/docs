# Renaming

The entire abap2UI5 project is developed under the `z2ui5` namespace. In certain scenarios, it may be necessary to rename the namespace to fit customer-specific requirements, such as:
* Different modules, e.g., z2ui5_sd, z2ui5_mm
* Custom namespaces, e.g., /ZZZ/, /YYY/
* Release-specific naming, e.g., z2411, z2502

abap2UI5 is compatible with the abaplint renaming feature and supports namespaces of up to 9 characters, e.g., `zabap2ui5`.

#### Functionality
To learn more about renaming ABAP artifacts, check out:
[Automagic standalone renaming of ABAP objects.](https://community.sap.com/t5/application-development-blog-posts/automagic-standalone-renaming-of-abap-objects/ba-p/13499851) 

Additional background information is available in the blog post:
[Renaming of ABAP Artifacts - The Power of abaplint & abapGit in ABAP Development.](https://www.linkedin.com/pulse/renaming-abap-artifacts-power-abaplint-github-actions-development-kqede/)
