---
outline: [2, 4]
---
# BTP Build Work Zone

Embed your abap2UI5 apps into BTP services like SAP Build Work Zone. The integration follows the usual BTP pattern for on-premise or cloud ABAP backends:

1. **Destination** — a BTP destination points to the HTTP service of the ABAP system that runs your abap2UI5 apps.
2. **Connector app** — a small standalone UI5 app from the [abap2UI5-addons](https://github.com/abap2UI5-addons) organization is deployed to BTP. It renders the abap2UI5 frontend and forwards every request through the destination to your ABAP backend.
3. **Work Zone content** — the connector app is added as content in SAP Build Work Zone and assigned to a page/role. Which abap2UI5 app a tile starts is controlled via the `app_start` parameter, so one deployed connector serves any number of app tiles.

All app logic stays in ABAP — nothing app-specific is deployed to BTP.

### Configuration

#### Destination

Create this destination in your BTP subaccount (**Connectivity → Destinations**):

- Name: `BACKEND_ABAP2UI5`
- Type: `HTTP`
- Description: abap2UI5 Destination
- URL: `https://<your-abap-system>/sap/z2ui5`
- Proxy Type: Internet
- Authentication: `BasicAuthentication`
- User: *(user)*
- Password: *(password)*

For on-premise systems without direct internet exposure, use Proxy Type `OnPremise` with the SAP Cloud Connector; for production, prefer a principal-propagation or SAMLAssertion setup over basic authentication.

#### Extra Properties

SAP Build Work Zone needs these additional properties to route requests correctly to your ABAP backend:

| Property | Value | Description |
|---|---|---|
| HTML5.DynamicDestination | `true` | Lets HTML5 apps resolve this destination at runtime |
| product.name | `ABAP System` | Names the backend type for the Work Zone tile configuration |
| sap-client | *(your client number)* | The SAP system client to connect to (e.g., `001`) |
| WebIDEEnabled | `true` | Enables the destination for SAP Business Application Studio |
| WebIDEUsage | `odata_abap,dev_abap` | Declares supported protocols for development tools |

#### Work Zone Setup

1. Subscribe to **SAP Build Work Zone** (standard edition is sufficient) in your subaccount and open the **Site Manager**.
2. Deploy the connector app from the [abap2UI5-addons](https://github.com/abap2UI5-addons) organization to your subaccount's HTML5 application repository.
3. In the **Content Manager**, add the deployed app, assign it to a group and to the `Everyone` role (or your own role), and add it to a site page.
4. Pass the abap2UI5 app class to start via the `app_start` parameter in the tile/target configuration — one deployment, one tile per app class.

After that, the tile opens your abap2UI5 app inside the Work Zone shell; the same content is also picked up by [SAP Mobile Start](/configuration/mobile_start).

### Further Reading
The original article series with step-by-step screenshots:
- [Installation & Configuration of BTP](https://www.linkedin.com/pulse/abap2ui5-integration-sap-business-technology-platform-13-installation-lf1re/)
- [Setup SAP Build Work Zone](https://www.linkedin.com/pulse/abap2ui5-integration-sap-business-technology-platform-23-setup-ujdqe/)

SAP's own tutorials cover the generic Work Zone plumbing: [Configure SAP Build Work Zone](https://developers.sap.com/tutorials/spa-configure-workzone..html) and [Integrate Your Application with SAP Build Work Zone](https://developers.sap.com/tutorials/integrate-with-work-zone..html).
