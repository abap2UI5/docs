---
outline: [2, 4]
---
# abapGit

This project uses [abapGit](https://github.com/abapGit/abapGit) for version control and easy code distribution.

## Installing abapGit

abapGit is itself ABAP, installed once per system before anything else in this
documentation can be pulled. The
[abapGit installation guide](https://docs.abapgit.org/user-guide/getting-started/install.html)
covers both flavors: the standalone report, which is one program to create and
activate and is enough to pull abap2UI5, and the developer version pulled with
it afterwards. ABAP Cloud systems — BTP ABAP Environment and S/4 Public Cloud —
use [abapGit for Eclipse](https://eclipse.abapgit.org/) in ADT instead; the
[S/4 Public Cloud](/configuration/s4_public_cloud) page walks through that pull
screenshot by screenshot. With abapGit in place, the
[Quickstart](/get_started/quickstart) is the next page.

## Git Client

We build all project artifacts directly in an ABAP system and store them in a GitHub repository through abapGit.
This makes the project easy to install on any customer system. We publish updates right away, and any user can pull them whenever they want.

## Apps as abapGit Artifacts

Beyond the abap2UI5 core framework, all abap2UI5 apps live as ABAP classes — abapGit-managed objects — so you can install them through abapGit without any manual deployment, unlike traditional UI5 apps.

You can also install multiple abap2UI5 apps with a single abapGit pull. This works smoothly in ABAP Cloud environments and cuts deployment overhead.

## Contribution

Contributions are welcome and easy to manage with pull requests on GitHub.
Open a pull request and commit your changes through abapGit.
