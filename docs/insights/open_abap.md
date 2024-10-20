---
outline: [2, 4]
---
# Setup

<i class="fa-brands fa-github"></i> [Repository](https://github.com/abap2UI5/setup)

abap2UI5 can be used with open-abap, providing the ability to run unit tests in GitHub Actions, perform frontend testing, and execute samples directly in your browser—without needing a backend.

<img width="800" alt="image" src="https://github.com/user-attachments/assets/4306fc51-a926-44e3-9572-e4f3fe0eb419">

### Functionality
* Downporting with [abaplint](https://abaplint.org/)
* Transpiling to JS with [abaplint/transpiler](https://github.com/abaplint/transpiler)
* Running on Node.js with [open-abap](https://github.com/open-abap/express-icf-shim)
* Service exposing via [express-icf-shim](https://github.com/open-abap/express-icf-shim)
* Browser Tests with [Playwright](https://playwright.dev/)
* Webpacking, Unit Testing...

### Tasks

Pull the repository and try out:

#### Downport & Transpile
```
npm run init
npm run build
```
#### Run Unit Tests
```
npm run unit
```
#### Run Webservice
```
npm run express
```
#### Run Playwright Tests
```
npm run init_play
npx playwright install --with-deps && npm i
npx playwright test
```

#### Webpack Build Strategy

1. Clone repositories into /src/
2. Downport /src/ into /downport/
3. Transpile with express-icf-shim into /output/
4. Webpack backend + frontend + database into folder build

```
npm run webpack:build
```