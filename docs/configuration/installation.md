---
outline: [2, 4]
---
# Installation
abap2UI5 apps run directly through an HTTP service — the fastest and simplest setup. It runs on any release and language version.

But production use may require extra considerations across different system environments.

### S/4 Public Cloud
In `S/4 Public Cloud`, the HTTP endpoint requires `S_DEVELOP` authorization, which makes it unsuitable for production. To make the app accessible to business users, install a separate frontend app and create an extra tile. See [S/4 Public Cloud](/configuration/s4_public_cloud) for details.

### S/4 Private Cloud
In `S/4 Private Cloud`, you can make the HTTP endpoint accessible to business users and manage extra authorizations. But to also use the SAP Fiori Launchpad, install an extra frontend app. See [Fiori Launchpad](/configuration/launchpad) for details.

### S/4 On-Premise
The setup in `S/4 On-Premise` matches `S/4 Private Cloud`. Follow the [Fiori Launchpad](/configuration/launchpad) guide.

### R/3 NetWeaver
The setup matches `S/4 Private Cloud`. Depending on your release level, you may also be able to add the apps to Launchpads. Follow the [Fiori Launchpad](/configuration/launchpad) guide.

### R/3 NetWeaver (<7.50)
For R/3 NetWeaver versions below 7.50, install the downported version with syntax adjustments for earlier releases. abap2UI5 runs on releases as old as 7.02. See [Downporting](/advanced/downporting) for details.

### BTP ABAP Environment
The setup in `BTP ABAP Environment` matches `S/4 Public Cloud`. Follow the [S/4 Public Cloud](/configuration/s4_public_cloud) guide.
