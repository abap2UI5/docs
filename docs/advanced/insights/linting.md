---
outline: [2, 4]
---
# Linting

The frontend JS code and the backend abap code are static code checked with every PR.

### abaplint

The project uses [abaplint](https://abaplint.app/) for static code analysis, ensuring code quality and adherence to ABAP best practices.

#### Configuration
You can review the project-specific abaplint configuration [here.](https://github.com/abap2UI5/abap2UI5/blob/main/ci/abaplint/abaplint.jsonc)

#### Dashboard
All statistics and results from the abaplint analysis are available on the abaplint dashboard [here.](https://abaplint.app/stats/abap2UI5/abap2UI5)

### ui5lint

The project uses the [ui5linter]((https://github.com/SAP/ui5-linter)) for static code analysis of frontend artifacts. The linter checks for issues such as deprecated API usage and ensures compatibility with UI5 2.x. 

#### Configuration
The default configuration is applied.