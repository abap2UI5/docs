# ajson

abap2UI5 relies heavily on JSON handling for frontend/backend communication, initially using `/UI2/CL_JSON`. Over time, various issues arose because certain functionalities were not available in specific releases and its limited documentation.

To address these challenges, abap2UI5 now uses the open-source project [ajson](https://github.com/sbcgua/ajson) for JSON handling. It is integrated into the project under the `z2ui5` namespace, ensuring that ajson is automatically installed with every abap2UI5 installation. This significantly reduces dependencies on SAP APIs.

Moreover, working with ajson is straightforward thanks to its clear API, excellent documentation, quick support for issue resolution, and best of all — it is compatible from version 702 to ABAP Cloud.
