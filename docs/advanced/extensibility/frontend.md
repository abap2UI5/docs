---
outline: [2, 4]
---
# Frontend

The abap2UI5 frontend artifacts are stored in the app folder. If you want to make adjustments, follow these steps:

#### Setup
Open VS Code or an editor of your choice, use the terminal:
```sh
git clone https://github.com/abap2UI5/abap2UI5
cd app
```
Replace the backend with your abap system in all three files:

<img width="400" alt="Frontend config files where the backend system URL must be replaced" src="https://github.com/user-attachments/assets/155c9a3f-8a0a-494b-8fc4-a4bba2bf0e90">


#### Develop & Test
Set the correct backend system in the yamls <br>
Replace "/sap/bc/z2ui5" with your endpoint in the manifest
```sh
npm i
npm run start-noflp
```
This is the output in the terminal:
<img width="1000" alt="npm run start-noflp output showing the local dev server running" src="https://github.com/user-attachments/assets/3e0118e8-d05f-48d1-bcc0-28073adf6e47">


#### Before PR (optional)
Transform UI5 App to stringified ABAP
```sh
cd ..
npm run auto_app2abap
```
