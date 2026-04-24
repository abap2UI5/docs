---
outline: [2, 4]
---
# Frontend

The abap2UI5 frontend artifacts live in the `app` folder. To adjust them, follow these steps:

#### Setup
Open VS Code or your preferred editor, then run in the terminal:
```sh
git clone https://github.com/abap2UI5/abap2UI5
cd app
```
Point all three files at your ABAP backend system:

<img width="400" alt="Frontend config files where the backend system URL must be replaced" src="https://github.com/user-attachments/assets/155c9a3f-8a0a-494b-8fc4-a4bba2bf0e90">

#### Develop & Test
Set the backend system in the YAML files.<br>
Replace `/sap/bc/z2ui5` with your endpoint in the manifest, then run:
```sh
npm i
npm run start-noflp
```
You should see output like this:
<img width="1000" alt="npm run start-noflp output showing the local dev server running" src="https://github.com/user-attachments/assets/3e0118e8-d05f-48d1-bcc0-28073adf6e47">

#### Before PR (Optional)
Convert the UI5 app to stringified ABAP:
```sh
cd ..
npm run auto_app2abap
```
