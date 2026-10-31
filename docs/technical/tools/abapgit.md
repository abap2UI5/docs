---
outline: [2, 4]
description: abapGit in abap2UI5 - installing it on-premise and in ADT, how the framework and every app travel through it, and how a contribution is committed with it.
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

## How the framework travels

The framework is developed in an ABAP system and serialized into its GitHub
repository with abapGit; a system installs it by pulling that repository, and
updates by pulling again. The folder logic is `PREFIX`, so the `src/00` to
`src/99` folders become sub-packages of the package you pick (see
[Namespaces, Renaming](/advanced/renaming#package-layout)).

## Apps as abapGit Artifacts

An abap2UI5 app is an ABAP class, so it is an abapGit object like any other: a
repository of app classes is pulled the same way as the framework, with no
separate frontend deployment, and one pull installs every class in it —
on-premise and, through abapGit for Eclipse, on ABAP Cloud.

## Contribution

Contributions are welcome and easy to manage with pull requests on GitHub.
Open a pull request and commit your changes through abapGit.
