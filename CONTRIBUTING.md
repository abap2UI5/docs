_This project is open source and developed alongside other projects or during free time. Contributions are greatly appreciated!_

Check out the contribution guidelines [here.](https://abap2ui5.github.io/docs/resources/contribution.html)

## Before opening a pull request

```bash
npm ci
npm run docs:dev       # the site at localhost, with hot reload, while writing
npm run check          # everything CI runs on a pull request
```

`npm run check` is `check.yml`: the test suite, then the VitePress build, then
the gates that hold the pages to what the other repositories actually ship —
the version the site names, the examples it prints, the Run buttons the
playground can honour, the API names and the sample links. A page that quotes
another repository is checked against that repository, not against memory,
which is why a green build is not on its own enough.

Nothing here is a source of truth about the framework: the API reference, the
sample catalogue and the corpus figures are generated or checked against
`abap2UI5`, `abap2UI5/samples` and its siblings. If a fact is wrong, it is
usually wrong there first. [AGENTS.md](AGENTS.md) is the full contract — how
the site is built, the eight gates, and what may be written where.
