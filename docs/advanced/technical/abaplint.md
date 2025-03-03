---
outline: [2, 4]
---

# abaplint  

This project uses [abaplint](https://abaplint.app/) for static code analysis and various additional tasks.  

#### Static Code Analysis  

With every PR, the ABAP code is checked using **abaplint**, ensuring code quality and adherence to ABAP best practices. You can review the project-specific abaplint configuration [here](https://github.com/abap2UI5/abap2UI5/abaplint.jsonc). All statistics and analysis results are available on the abaplint app dashboard [here](https://abaplint.app/stats/abap2UI5/abap2UI5).  

#### Downporting  

An **abap2UI5** version with **compatible syntax for ABAP v702** is automatically built using the abaplint downport feature:  

```sh
abaplint --fix .github/abaplint/abap_702.jsonc
```

#### Renaming
All artifacts are compatible with the abaplint renaming feature, allowing you to use abap2UI5 under different custom namespaces:

```sh
abaplint .github/abaplint/rename_test.jsonc --rename
```
