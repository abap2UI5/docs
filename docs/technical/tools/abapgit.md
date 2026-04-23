---
outline: [2, 4]
---
# abapGit

This project uses [abapGit](https://github.com/abapGit/abapGit) to manage version control and distribute its code efficiently.

#### Git Client

We develop all project artifacts directly in an ABAP system and store them in a GitHub repository via abapGit.
This setup makes the project easy to install on any customer system. We distribute updates immediately, and any user can pull them at any time.

#### Apps as abapGit Artifacts

In addition to the abap2UI5 core framework, all abap2UI5 apps live as ABAP classes — abapGit-managed objects — so you can install them via abapGit without any manual deployment, unlike traditional UI5 apps.

You can also install multiple abap2UI5 apps with a single abapGit pull. This works seamlessly in ABAP Cloud environments, reducing deployment overhead.

#### Contribution

Contributions are welcome and easily managed through pull requests on GitHub.
Create a new pull request and commit your changes using abapGit.
