---
outline: [2, 4]
---
# Frontend

The abap2UI5 frontend lives in the framework repository, in two folders:

| Folder | What it is |
|---|---|
| `bundle/webapp` | the frontend itself: the component, the actions, the custom controls, the developer tools. It is no app of its own - the backend serves it as one script, the bundle a page asks for with `?z2ui5-bundle` (the custom controls are a bundle of their own, `?z2ui5-bundle=cc`) |
| `app_source/webapp` | the minimal UI5 app that runs abap2UI5: one view with one embed control, which loads the bundle from the backend. The page the node answers a GET with and the BSP `Z2UI5` are written from it |

Nothing under `src/01/03` or `src/03/02` is edited by hand: those are the
generated ABAP copies of the two folders.

## Setup
Open VS Code (or your editor of choice), then run in the terminal:
```sh
git clone https://github.com/abap2UI5/abap2UI5
cd abap2UI5
npm ci
```

## Build & Test
The framework also runs in Node, transpiled, which is the quickest way to see a
change. Build the transpiled backend once, then start the development server
that answers `?z2ui5-bundle` from `bundle/webapp` as it is on disk:
```sh
npm run downport && npm run auto_transpile
npm run express:live
```
Open `http://localhost:3000`, edit a file under `bundle/webapp` and reload the
page. The page itself and its app (`app_source/webapp`) come from the built
backend and are not live.

To try a change against an SAP system instead, `app_source` is a UI5 project of
its own: `npm install` and `npm start` in that folder, with the system named in
its `ui5.yaml`.

## Before PR (Optional)
Write the change into the generated ABAP classes and the BSP:
```sh
npm run app2abap
```
It formats `bundle/webapp`, regenerates `src/01/03` and `src/03/02`, and
normalizes the result, exactly as CI does. Commit the regenerated files
together with the change.
