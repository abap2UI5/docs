---
outline: [2, 4]
---
# Renaming

The entire abap2UI5 project lives under the `z2ui5` namespace. You may need to rename it to fit customer-specific requirements, like:
- Different modules, e.g., `z2ui5_sd`, `z2ui5_mm`
- Custom namespaces, e.g., `/ZZZ/`, `/YYY/`
- Release-specific naming, e.g., `z2411`, `z2502`
- One installation per app, e.g., `z2ui5app1`, `z2ui5app2`

abap2UI5 works with the abaplint renaming feature and supports namespaces up to 9 characters, e.g., `zabap2ui5`.

#### Why Rename?
ABAP stacks have no package manager: every object pulled with abapGit is created globally, so a system can hold only one version of abap2UI5 — and every app in the system is forced to use it. Pulling the latest version can then break existing apps. Public API changes are kept to a minimum, but a constantly evolving project cannot avoid them entirely.

Renaming closes this gap. It is not a true package management system, but it lets you install abap2UI5 multiple times under different namespaces in the same system and upgrade each installation at its own pace. At the app level, you simply implement the renamed app interface — e.g., `z2ui5_sd_if_app` or `z2411_if_app` instead of `z2ui5_if_app`.

#### How It Works
[abaplint](https://abaplint.org) can rename ABAP artifacts across a whole repository: you define rename patterns (old name → new name, including regular expressions) in an abaplint configuration, and `abaplint --rename` rewrites every class, interface, and reference consistently, writing the result to an output folder:

```jsonc
"rename": {
  "output": "output",
  "patterns": [
    { "type": "CLAS|INTF", "oldName": "z2ui5(.*)", "newName": "zmyns$1" }
  ]
}
```

The renamed copy is a complete, installable abapGit project under your own namespace — install it side by side with the original, pin it to a release, or ship it inside your product. The abap2UI5 CI runs this transformation on every change (`npm run rename`, workflow `test_rename.yaml`) to guarantee the codebase stays renameable.

#### Step-by-Step Guide
Everything is already set up in the main repository: the on-demand GitHub Action `build_rename` renames all artifacts to a namespace of your choice and pushes the result as a ready-to-install branch. Renaming abap2UI5 takes just two steps:

1. **Fork** the [abap2UI5 repository](https://github.com/abap2UI5/abap2UI5)
2. **Run the Action** — in your fork, open the *Actions* tab (enable workflows when asked), select the **build_rename** workflow, and start it with your new namespace (max. 9 characters, e.g., `ZMYUI5`)

The workflow runs `abaplint --rename` with the checked-in configuration `.github/abaplint/rename.jsonc` and pushes the renamed sources to the branch `rename_<name>` (e.g., `rename_zmyui5`). The branch contains the complete renamed `src` tree together with a matching `.abapgit.xml` — **pull it with abapGit** into your ABAP system for a parallel installation next to the original.

To upgrade an installation later, sync your fork with upstream and re-run the workflow with the same name: the branch is updated to the current state (nothing is pushed when there are no content changes), and you simply pull again with abapGit.

#### Renaming in Practice: ajson
abap2UI5 itself relies on this feature: its JSON handling comes from the open-source project [ajson](https://github.com/sbcgua/ajson), which is integrated under the `z2ui5` namespace via renaming — so there are no collisions if you pull both abap2UI5 and ajson separately into the same system. A GitHub Action in the [mirror-ajson](https://github.com/abap2UI5/mirror-ajson) repository checks weekly for upstream changes and automatically creates a pull request with the latest ajson version renamed to `z2ui5`. abapGit bundles ajson under its own namespace the same way — renaming with abaplint also makes it possible to integrate open-source projects into each other.

#### Further Reading
- [Automagic standalone renaming of ABAP objects](https://community.sap.com/t5/application-development-blog-posts/automagic-standalone-renaming-of-abap-objects/ba-p/13499851)
- [Renaming of ABAP Artifacts — The Power of abaplint and abapGit in ABAP Development](https://www.linkedin.com/pulse/renaming-abap-artifacts-power-abaplint-github-actions-development-kqede/)
