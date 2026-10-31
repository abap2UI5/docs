---
outline: [2, 3]
description: Running an abap2UI5 app inside the object page of a Fiori elements app with the embed control z2ui5.embed.Container from npm - the package, the fragment, the startup parameters and the abap2UI5 release it needs.
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
controller code. The control itself runs against **abap2UI5 1.145.0 or
later**, whose service answers `?z2ui5-bundle`; inside a Fiori elements object
page it needs **1.146.0 or later**, because from that release on an embedded
abap2UI5 leaves the URL hash to the page it runs in. 1.145.0 clears the hash
after every roundtrip, and the object page goes back to the list. The
control's README lists the other things 1.145.0 does not do yet.

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

## Running it in production

Three questions decide whether an embedded section goes live: who may start
which class, what an upgrade of abap2UI5 does to the apps already running, and
what several containers on one page cost the server.

### Who may start which class

The class name and the parameters are in the fragment, and the fragment runs
in the browser. Whoever can reach the service node can send another class name
or another key - exactly as with `?app_start=` on abap2UI5's own page, so
embedding the app changes nothing here. The framework checks the type and not
the user: a class that does not implement `z2ui5_if_app` is refused before it
is instantiated, and a draft belongs to the user who created it. Who may run
what is decided in two places, and both are ordinary ABAP:

- **The node decides who reaches abap2UI5 at all.** Logon procedure, SSO and
  the authorization field of the node in `SICF` (checked against `S_ICF`)
  work as on any other service. A node of its own for the apps of one area -
  a handler class that calls `z2ui5_cl_ui5_http_handler=>run( server )` - is
  named in the control's `endpoint` property:
  `<z2ui5:Container endpoint="/sap/bc/z2ui5_sd" .../>`. Give every container
  on a page the same endpoint: the first one loads the frontend for all of
  them.
- **The class decides who may run it.** An `AUTHORITY-CHECK` in `main( )`,
  before anything is selected or displayed, with a message when it fails. It
  is the one check that holds however the class is started - its own page,
  the launchpad, a container, or `nav_app_call( )` from another app. The
  node cannot make this decision for it: the first POST carries the class in
  its body, and the roundtrips after it carry none. See
  [Authorization](/configuration/authorization).

The parameters are user input too. A credit check started with
`salesorder = 4711` can be started with any other number, so the class checks
the object, not only the app - the authorization for the sales organization of
the order, for example. Reading through a CDS view entity with an access
control applies that access control, and EML on the RAP business object runs
its authorization control; a plain `SELECT` on the database table checks
nothing.

```abap
METHOD z2ui5_if_app~main.

  IF client->check_on_init( ).

    AUTHORITY-CHECK OBJECT `Z_APP_AUTH`
                    ID `APP` FIELD `ZCL_SALES_ORDER_CREDIT_CHECK`.
    IF sy-subrc <> 0.
      client->message_box_display( text  = `You are not authorized to use this app.`
                                   type  = `error`
                                   title = `Not authorized` ).
      RETURN.
    ENDIF.

    " read the order through a view with access control (or EML), and
    " refuse the same way when the user may not see it

  ENDIF.

ENDMETHOD.
```

### Upgrades

An upgrade of abap2UI5 is a transport like any other: an abapGit pull on the
development system, then the usual way to production. Nothing changes before
that, and an embedded section needs no test process of its own - it gets the
same check on the quality system as a SAPUI5 upgrade or a support package,
which move every Fiori app of the system at once as well. The frontend comes
from the backend, so browser and server always run the same version.

Pin a [release](https://github.com/abap2UI5/abap2UI5/releases/) rather than
tracking `main` ([Productive Usage](/configuration/productive_usage)). To keep
the embedded sections on a fixed version while development moves on, install
abap2UI5 a second time under another prefix ([Renaming](/advanced/renaming))
with a node of its own, and point the control's `endpoint` at it.

### Sessions and load

An abap2UI5 app is stateless by default
([Statefulness](/cookbook/expert_more/statefulness)): between two clicks there
is no open ABAP session and no work process held. What a container keeps on the
server is its draft - one row in `Z2UI5_T_01`, written on each roundtrip and
deleted when it expires (after four hours by default, `draft_exp_time_in_hours`
in the [user exit](/advanced/extensibility/user_exits)). Three containers on a
page are three drafts, not three sessions.

What a page costs:

| When | Requests |
|---|---|
| The page opens | one GET for the frontend, for the whole page (a `304` once the browser has it), and one POST per container to start its app |
| The user acts in a container | one POST, from that container only |
| Another object is opened | one POST per container - a new start with the new key |
| Nobody touches the section | nothing |

The server time of a roundtrip is the app's `main( )` plus reading and writing
its draft, so a section is as fast as its start is cheap: read what the section
shows when it starts, and leave the expensive part to a button. The
[roundtrip monitor](/configuration/monitoring) measures it per class - total
time, draft load, `main( )`, response - and the Admin Cockpit shows the p95 per
app, which is the number to look at on the quality system with the real data.
*From abap2UI5 1.147.0 on.*

Do not switch an embedded app to `set_session_stateful( )`: that is the one
setting that turns a container into an open session, one work process per user
and container for as long as the page stays open.

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

Before the control, the add-on `abap2UI5-addons/fiori-elements-integration` (no
longer published) embedded abap2UI5 by hand: a controller extension registered
the launchpad integration, created the `z2ui5` component with `Component.create`
and put it into a `VBox` in a `ComponentContainer`, and a launchpad target
mapping for `z2ui5` had to exist. The control replaces all of it - a fragment
bound to the object, no controller code, no target mapping - and the parameters
go in by name rather than by position. Move an app built that way to the
control.
