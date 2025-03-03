# ajson

abap2UI5 relies heavily on JSON handling for frontend/backend communication, initially using `/UI2/CL_JSON`. Over time, various issues arose because certain functionalities were not available in specific releases and its limited documentation.

To address these challenges, abap2UI5 uses the open-source project [ajson](https://github.com/sbcgua/ajson) for JSON handling. 

Using ajson significantly reduces dependencies on SAP APIs. Moreover, it provides a straightforward and reliable approach to JSON handling, thanks to its clear API, excellent documentation, quick support for issue resolution, and best of all — it is compatible from version 702 to ABAP Cloud.

#### Integration

It is integrated into the project under the `z2ui5` namespace and automatically installed with every abap2UI5 installation. You can access it directly using:
```abap
z2ui5_cl_ajson=>
```

#### Updates

Every update and bug fix in ajson is automatically reflected in abap2UI5 via GitHub Actions and the [mirror-ajson](https://github.com/abap2UI5/mirror-ajson) repository, ensuring always using the latest version.
