# #26 A Tile Like Any Other

An app that is reached by pasting a URL is a demo. It becomes an application
when it has a tile — and in the launchpad an abap2UI5 app is indistinguishable
from the RAP and freestyle tiles beside it.

**What is installed once.** The launchpad loads a UI5 app from the system's
UI5 repository, so the abap2UI5 shell has to exist there as one: the
[launchpad connector](https://github.com/abap2UI5-addons) ships it through
abapGit, as app `z2ui5`. That is one deployment for the whole system, not one
per app — every abap2UI5 tile points at the same shell.

**What is configured per app.** A target mapping, with the app class as the
parameter:

| | |
|---|---|
| Semantic Object | `Z2UI5_CL_MY_APP` |
| Action | `display` |
| URL | `/sap/bc/ui5_ui5/sap/z2ui5` |
| Parameter | `app_start` / `Z2UI5_CL_MY_APP` |

A tile, a catalogue, a role — the Fiori administration the system already does,
with nothing abap2UI5-specific in it except the parameter.

**What the app can do inside.** It knows where it is:

```abap
    IF client->get( )-check_launchpad_active = abap_true.
      client->follow_up_action( val   = client->cs_event-set_title_launchpad
                                t_arg = VALUE #( ( `Job Monitor` ) ) ).
    ENDIF.
```

Startup parameters from the target mapping arrive in
`client->get( )-t_comp_params`, and navigation to another *Fiori* app goes
through the launchpad's own cross-app navigation, so the shell's back button
keeps working.

**What to do when the tile is blank.** The one recurring trouble is not the
app but the UI5 app index after an abapGit import: run
`/UI5/APP_INDEX_CALCULATE`, clear the HTTP cache in `SMICM`, hard-reload. The
[launchpad page](/configuration/launchpad) walks through it with screenshots.

Public Cloud is a different door: there the launchpad is Build Work Zone, and
the [ABAP Cloud pages](/configuration/btp) describe the setup.

One shell in the UI5 repository, one parameter per tile. No user can tell
the difference.

Happy ABAPing! 🦖🦕🦣
