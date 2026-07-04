---
outline: [2, 4]
---
# Frontend

The abap2UI5 frontend artifacts live in the `app` folder. To adjust them:

#### Setup
Open VS Code (or your editor of choice), then run in the terminal:
```sh
git clone https://github.com/abap2UI5/abap2UI5
cd abap2UI5/app
```
Next replace `<<system>>` in the three config files — `ui5.yaml`, `ui5-local.yaml` and `ui5-mock.yaml` — so they all point to your ABAP backend system:

<img width="400" alt="Frontend config files where the backend system URL must be replaced" src="https://github.com/user-attachments/assets/155c9a3f-8a0a-494b-8fc4-a4bba2bf0e90">

#### Build & Test
If your HTTP service uses a different path, also replace `/sap/bc/z2ui5` with your endpoint in the manifest. Then run:
```sh
npm i
npm run start-noflp
```
You'll see output like this:
<img width="1000" alt="npm run start-noflp output showing the local dev server running" src="https://github.com/user-attachments/assets/3e0118e8-d05f-48d1-bcc0-28073adf6e47">

#### Before PR (Optional)
Convert the UI5 app to stringified ABAP:
```sh
cd ..
npm run auto_app2abap
```
