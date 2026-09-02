# Where the Selection Screen Went

Before anyone called it full-stack, ABAP had this:

```abap
REPORT zre_app_input.
  PARAMETERS pa_arbgb TYPE t100-arbgb DEFAULT `MDG_TR`.
START-OF-SELECTION.
  MESSAGE |Input: { pa_arbgb }| TYPE `I`.
```

Four lines, and every layer is present. A screen exists. It has a labelled,
typed input field with a default and value help. It validates. It handles the
event. Nobody wrote a frontend, because declaring the variable *was* writing the
frontend.

That trade — you name the data, the system draws the screen — is what made ABAP
productive for thirty years, and it is what disappeared on the way to the
browser. The variable and the field it appears in became two artefacts in two
places, maintained by two people, kept in step by a service in between.

abap2UI5 puts them back together with `_bind`:

```abap
    )->tag( `Input`
        )->a( n = `value` v = client->_bind( pa_arbgb )
```

The attribute is not a field name in a string. It is the ABAP variable, handed
over by reference; the framework builds the model around it, ships it, takes the
user's input back and puts it where it came from. What the developer maintains
is a variable, in a class, next to the code that reads it.

It is not the same deal as `PARAMETERS`, and pretending otherwise would be
dishonest — the layout is written by hand here, because the screen is UI5 and
UI5 has more to say than a selection screen did. What comes back is the other
half: one place where the data lives, and no service between the field and the
variable.

**The selection screen's best idea was never the screen. It was that the
variable and the field were the same declaration.**

Happy ABAPing! 🦖🦕🦣
