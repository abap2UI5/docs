---
outline: [2, 4]
---
# abaplint

This project uses [abaplint](https://abaplint.app/) for static code analysis and other tasks.

#### Static Code Analysis

With every PR, **abaplint** checks the ABAP code to ensure quality and adherence to ABAP best practices. Review the project-specific [abaplint configuration](https://github.com/abap2UI5/abap2UI5/blob/main/abaplint.jsonc). All statistics and analysis results live on the [abaplint app dashboard](https://abaplint.app/stats/abap2UI5/abap2UI5).

#### Downporting

The abaplint downport feature automatically builds an **abap2UI5** version with **syntax compatible with ABAP 7.02**:

```sh
abaplint --fix .github/abaplint/abap_702.jsonc
```

#### Renaming
All artifacts are compatible with the abaplint renaming feature, letting you use abap2UI5 under different custom namespaces:

```sh
abaplint .github/abaplint/rename_test.jsonc --rename
```
