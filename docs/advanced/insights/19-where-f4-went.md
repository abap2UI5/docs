# #19 Where F4 Went

`PARAMETERS pa_arbgb TYPE t100-arbgb` got a value help for free, because the
DDIC knew the search help and the screen knew the DDIC. On the way to the
browser that link broke: an `Input` in a UI5 view has no idea what type stands
behind it.

abap2UI5 does not put the automatic version back. It gives the three shapes an
F4 actually comes in, each one a few lines, from cheapest to widest.

**Suggestions while typing.** The candidates are an internal table bound to
the input; UI5 filters it in the browser, and no roundtrip is involved after
the first render:

```abap
    )->ele( `Input`
        )->a( n = `value`           v = client->_bind( country )
        )->a( n = `suggestionItems` v = client->_bind( t_countries )
        )->a( n = `showSuggestion`  b = abap_true
        )->ele( `suggestionItems`
            )->tag( n = `ListItem` ns = `core`
                )->a( n = `text`           v = `{CODE}`
                )->a( n = `additionalText` v = `{NAME}` ).
```

**A selection popup.** The input shows the value-help icon and raises an event
when it is pressed. The handler runs the `SELECT`, fills a table, and opens a
popup with a list in it. The row the user picks is an event with an argument,
and the handler writes it into the bound attribute:

```abap
    )->tag( `Input`
        )->a( n = `value`            v = client->_bind( carrid )
        )->a( n = `valueHelpRequest` v = client->_event( `F4` )
        )->a( n = `showValueHelp`    b = abap_true ).
```

No sub-app, no navigation — the popup is a second view of the same class, so
the chosen row is simply an attribute.

**A reusable value help.** When the same F4 is needed from ten screens, it
becomes a class: a `z2ui5_if_app` that displays into the popup slot, called
with `nav_app_call( )`, and read back through `get_app_prev( )` after it
leaves — the stack from [#18](/advanced/insights/18-call-screen-leave-screen).
Written once, the DDIC search help it wraps is whatever `SELECT` it runs.

What is honestly gone is the part where nothing had to be written. The screen
does not know the type behind the field any more, so the developer names the
candidates. What came back is that the candidates can be anything an ABAP
`SELECT` can produce — including a table that did not exist a millisecond
ago, which is where [#1](/advanced/insights/01-somewhere-on-the-way-to-ui5) started.

F4 is three lines and a SELECT now. It is no longer free, and it is no
longer limited to the DDIC either.

Happy ABAPing! 🦖🦕🦣
