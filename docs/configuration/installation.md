---
outline: [2, 4]
---

# Installation
abap2UI5 apps run directly through an HTTP service, making this the fastest and most straightforward setup option. It works on any release and language version.

However, for productive usage, additional considerations may be required for different system environments.

### S/4 Public Cloud
In `S/4 Public Cloud`, the HTTP endpoint is accessible only with `S_DEVELOP` authorization, which makes it unsuitable for productive use. To make this app accessible for business users, you can install a separate frontend app and create an additional tile. Find more information [here.](/configuration/s4_public_cloud)

### S/4 Private Cloud
In `S/4 Private Cloud` you can make the HTTP endpoint accessible for business users and manage additional authorizations. However, if you want to use the SAP Fiori Launchpad as well, you can install an additional frontend app. Find all the information [here.](/configuration/launchpad)

### S/4 On-Premise
The setup in `S/4 On-Premise` is the same as in `S/4 Private Cloud`. Follow the guide [here.](/configuration/launchpad)

### R/3 NetWeaver
The setup is the same as in `S/4 Private Cloud`. Depending on your release level, you may be able to add the apps to Launchpads. Follow the guide [here.](/configuration/launchpad)

### R/3 NetWeaver (<7.50)
For older versions of R/3 NetWeaver lower v7.50, install the downported version with syntax adjustments for earlier releases. abap2UI5 runs on versions down to 7.02. See the downport section for more information [here.](/advanced/downporting)

### BTP ABAP Environment
The setup in `BTP ABAP Environment` is the same as in `S/4 Public Cloud`. Follow the guide [here.](/configuration/s4_public_cloud)
