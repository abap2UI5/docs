---
outline: [2, 3]
---
# Fiori Elements Integration

An abap2UI5 app can run inside the object page of a Fiori elements app, next
to the sections the annotations make. The control `z2ui5.embed.Container`
from the npm package
[`@abap2ui5/embed-control`](https://www.npmjs.com/package/@abap2ui5/embed-control)
does the work: it sits in a fragment, is bound to the object on the page, and
hands fields of that object to the ABAP class as startup parameters.

<img width="747" height="387" alt="abap2UI5 app embedded in Fiori Elements object page" src="https://github.com/user-attachments/assets/c14d5732-3b8c-4fa5-83ab-6d188a4d87db" />

The control loads the abap2UI5 frontend itself, from the service it talks to
anyway (`/sap/bc/z2ui5?z2ui5-bundle`), so the frontend always has the version
of the backend. No launchpad target mapping for `z2ui5` is needed, and no
controller code. It needs **abap2UI5 1.146.0 or later**: from that release
on, an embedded abap2UI5 leaves the URL hash to the page it runs in. An older
one clears the hash after every roundtrip, and the object page goes back to
the list.

## The package

The control comes from npm like any dependency, and is named in three places
of the Fiori elements app:

| File | |
|---|---|
| `package.json` | the dependency: `npm install @abap2ui5/embed-control` |
| `ui5.yaml` | `includeDependency`, so that `ui5 build` takes the control into `dist/thirdparty/z2ui5/embed/` |
| `webapp/manifest.json` | the resourceRoot of the namespace `z2ui5.embed`: `./thirdparty/z2ui5/embed/` |

```yaml
# ui5.yaml
builder:
  settings:
    includeDependency:
      - "@abap2ui5/embed-control"
```

```json
"sap.ui5": {
  "resourceRoots": {
    "z2ui5.embed": "./thirdparty/z2ui5/embed/"
  }
}
```

`ui5 serve` serves the control without the entry in `ui5.yaml`; the build
copies it only when it is named there. The abap2UI5 frontend is not part of
the build. For `ui5 serve`, list every UI5 library the embedded ABAP apps use
under `framework/libraries` of `ui5.yaml`, not only the ones of the Fiori
elements app - a view with a `SimpleForm` needs `sap.ui.layout`.

Deployed to the system that runs abap2UI5, or behind an application router
that routes `/sap/bc/z2ui5` to it, the app needs nothing else.

## OData V4: a custom section

Fiori elements for OData V4 takes content of its own as a custom section of
the object page: an entry in `content.body.sections` of the page's settings in
`manifest.json`, and a fragment.

```json
"CustomersObjectPage": {
  "type": "Component",
  "id": "CustomersObjectPage",
  "name": "sap.fe.templates.ObjectPage",
  "options": {
    "settings": {
      "contextPath": "/Customers",
      "content": {
        "body": {
          "sections": {
            "abap2UI5": {
              "template": "demo.fe.ext.Abap2UI5Section",
              "title": "abap2UI5",
              "position": {
                "placement": "After",
                "anchor": "General"
              }
            }
          }
        }
      }
    }
  }
}
```

The fragment `webapp/ext/Abap2UI5Section.fragment.xml` holds the control:

```xml
<core:FragmentDefinition
    xmlns:core="sap.ui.core"
    xmlns:z2ui5="z2ui5.embed">
    <z2ui5:Container
        core:require="{ Section: 'demo/fe/ext/Abap2UI5Section' }"
        app="{ path: 'ID', formatter: 'Section.app' }"
        params="{ path: 'ID', targetType: 'any', formatter: 'Section.params' }"
        height="420px"/>
</core:FragmentDefinition>
```

and `webapp/ext/Abap2UI5Section.js` decides which ABAP class runs, and with
which parameters:

```js
sap.ui.define([], () => {
  "use strict";

  return {
    app(id) {
      return id ? "Z2UI5_CL_UI5_APP_HI_WORLD" : "";
    },

    params(id) {
      return id ? { customer: id } : null;
    },
  };
});
```

- The section is bound to the object, so `ID` is the key of the customer on
  the page. Nothing starts before the page has one, and another customer ends
  the running abap2UI5 session and starts a new one with its key.
- `targetType: 'any'` is needed for `params`. An OData V4 binding converts
  the value of a service property into the type of the control property, and
  an `Edm.String` does not convert into the object `params` is ("Don't know
  how to format String to object"). With it, the formatter gets the key as it
  is.
- The control needs a height - a section gives it none of its own.

## OData V2: an object page extension

The templates for OData V2 (`sap.suite.ui.generic.template`) take content of
their own as a view extension of the object page, in
`sap.ui5/extends/extensions/sap.ui.viewExtensions` of `manifest.json`:

```json
"extends": {
  "extensions": {
    "sap.ui.viewExtensions": {
      "sap.suite.ui.generic.template.ObjectPage.view.Details": {
        "AfterFacet|Countries|General": {
          "type": "XML",
          "className": "sap.ui.core.Fragment",
          "fragmentName": "demo.fev2.ext.Abap2UI5Section",
          "sap.ui.generic.app": {
            "title": "abap2UI5"
          }
        }
      }
    }
  }
}
```

`AfterFacet|Countries|General` places a section of its own after the facet
with the id `General` of the entity set `Countries` - the id the facet has in
the metadata extension of the RAP service. The fragment hands three fields of
the country to the app:

```xml
<core:FragmentDefinition
    xmlns:core="sap.ui.core"
    xmlns:z2ui5="z2ui5.embed">
    <z2ui5:Container
        core:require="{ Section: 'demo/fev2/ext/Abap2UI5Section' }"
        app="{ path: 'Country', formatter: 'Section.app' }"
        params="{ parts: [ 'Country', 'Language', 'Nationality' ], formatter: 'Section.params' }"
        height="420px"/>
</core:FragmentDefinition>
```

```js
sap.ui.define([], () => {
  "use strict";

  const APP = "Z2UI5_CL_UI5_APP_HI_WORLD";

  return {
    app(country) {
      return country ? APP : "";
    },

    params(country, language, nationality) {
      return country ? { country, language, nationality } : null;
    },
  };
});
```

- An OData V2 binding hands over the values as they are, so a composite
  binding with a formatter is all `params` needs - no `targetType`.
- The templates keep the object page and bind it to the next object, so the
  control stays on the page: another country ends the running abap2UI5
  session, and the control starts a new one in place with the new fields.
- The control needs a height here too.

## The ABAP class

The class reads the parameters with `client->get( )-t_comp_params` - a
table of name and value, with the names the formatter gave them. Read them
once, when the app starts:

```abap
METHOD z2ui5_if_app~main.
  IF client->check_on_init( ).
    " params="{ country: 'AT', language: 'EN', nationality: 'Austrian' }"
    LOOP AT client->get( )-t_comp_params INTO DATA(param).
      IF to_lower( param-n ) = `country`.
        country = param-v.
      ENDIF.
    ENDLOOP.
    " ... read the country, build the view
  ENDIF.
ENDMETHOD.
```

Every value arrives as a string. The table also carries `app_start`, the class
the formatter named.

## Examples

Both examples are developed in
[abap2UI5/samples-embed-control](https://github.com/abap2UI5/samples-embed-control),
where they take the package from npm, and run without an SAP system as well:
their OData service comes from a mock server, abap2UI5 from
[`@abap2ui5/node-runtime`](https://www.npmjs.com/package/@abap2ui5/node-runtime).

- [`fiori-elements`](https://github.com/abap2UI5/samples-embed-control/tree/main/fiori-elements) -
  OData V4, list report and object page of customers, the control in a
  custom section.
- [`fiori-elements-v2`](https://github.com/abap2UI5/samples-embed-control/tree/main/fiori-elements-v2) -
  OData V2, list report and object page of countries, the control in an
  object page extension. Its folder `abap/` holds the RAP service the app
  reads and the abap2UI5 app it starts: the CDS view entity on `T005T`, its
  metadata extension, the service definition, the OData V2 binding and the
  class `Z2UI5_CL_EMBED_COUNTRY`, which shows the startup parameters and
  reads the country again from the view.

### On a system: the branch `rap`

The branch
[`rap`](https://github.com/abap2UI5/samples-embed-control/tree/rap) of
[abap2UI5/samples-embed-control](https://github.com/abap2UI5/samples-embed-control)
installs the OData V2 example with one abapGit pull: the RAP service, the
class, and the app as the BSP `Z2UI5_HOST_FE`, with the control from npm.

1. Install abap2UI5 1.146.0 or later and activate its HTTP service
   `/sap/bc/z2ui5`.
2. Pull the branch `rap` with abapGit into a new package.
3. Publish the service binding `Z2UI5_UI_EMBED_COUNTRY_O2` (ADT: open it,
   **Publish**) if the pull did not, and activate the ICF nodes
   `/sap/bc/ui5_ui5/sap/z2ui5_host_fe` and `/sap/bc/bsp/sap/z2ui5_host_fe`
   in `SICF`.
4. Open `/sap/bc/ui5_ui5/sap/z2ui5_host_fe/index.html` and pick a country.

It needs RAP view entities and OData V2 bindings - SAP S/4HANA 2020 (ABAP
7.55) or later. `T005T` is not a released API, so the view is for standard
ABAP, not ABAP Cloud.

## Superseded: `Component.create` in a controller extension

Before the control, the add-on
[abap2UI5-addons/fiori-elements-integration](https://github.com/abap2UI5-addons/fiori-elements-integration)
embedded abap2UI5 by hand: a controller extension registered the launchpad
integration, created the `z2ui5` component with `Component.create` and put it
into a `VBox` in a `ComponentContainer`, and a launchpad target mapping for
`z2ui5` had to exist. The control replaces all of it - a fragment bound to
the object, no controller code, no target mapping - and the parameters go in
by name rather than by position. Move an app built that way to the control.
