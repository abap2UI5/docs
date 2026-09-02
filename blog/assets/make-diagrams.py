#!/usr/bin/env python3
"""The diagrams for the abap2UI5 Know-How series.

Run from the repository root:  python3 blog/assets/make-diagrams.py
Writes blog/assets/diagrams/*.svg, one per article that needs one.

WHY SVG AND NOT PNG. These are boxes, arrows and words - the one thing a
raster format is worst at. An SVG stays sharp at any width, diffs as text when
a label changes, and costs a few kB instead of a few hundred.

WHY EACH ONE PAINTS ITS OWN BACKGROUND. A docs page can be light or dark, and
an SVG embedded through <img> cannot see the site's theme class - only
prefers-color-scheme, which disagrees with the site the moment a reader uses
the theme toggle against their system setting. So every diagram is a
self-contained cream card with its own border: one appearance, correct on both.

THE PALETTE IS THE BRAND'S, not a new one (docs/resources/logo.md):
red #D03C4A for what the article is about, dark red for its emphasis, ink for
structure, muted for everything the reader is not being pointed at.
"""
import os

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'diagrams')
R, R2, RL = '#D03C4A', '#A83232', '#F6E3E5'
CREAM, SAND, WHITE = '#FAF2EC', '#F0DFD2', '#FFFFFF'
INK, MUTED, LINE = '#3A2A2E', '#9C8A8E', '#D8C7BC'
FONT = ("-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,"
        "'Helvetica Neue',Arial,sans-serif")
MONO = "ui-monospace,SFMono-Regular,Menlo,Consolas,'Liberation Mono',monospace"


def esc(t):
    return t.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')


def box(x, y, w, h, label, sub=None, accent=False, dashed=False, mono=False):
    fill = RL if accent else WHITE
    stroke = R if accent else LINE
    dash = ' stroke-dasharray="5 4"' if dashed else ''
    o = [f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="7" fill="{fill}" '
         f'stroke="{stroke}" stroke-width="1.5"{dash}/>']
    fam = MONO if mono else FONT
    size = 12.5 if mono else 13.5
    cy = y + h / 2 + (0 if sub is None else -7) + 4.5
    o.append(f'<text x="{x + w / 2}" y="{cy}" font-family="{fam}" font-size="{size}" '
             f'font-weight="{600 if accent else 500}" fill="{R2 if accent else INK}" '
             f'text-anchor="middle">{esc(label)}</text>')
    if sub:
        o.append(f'<text x="{x + w / 2}" y="{cy + 17}" font-family="{FONT}" '
                 f'font-size="11.5" fill="{MUTED}" text-anchor="middle">{esc(sub)}</text>')
    return ''.join(o)


def arrow(x1, y1, x2, y2, label=None, accent=False, above=True, dashed=False):
    c = R if accent else MUTED
    dash = ' stroke-dasharray="5 4"' if dashed else ''
    o = [f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="{c}" '
         f'stroke-width="1.6" marker-end="url(#a{"r" if accent else "m"})"{dash}/>']
    if label:
        mx, my = (x1 + x2) / 2, (y1 + y2) / 2
        o.append(f'<text x="{mx}" y="{my + (-9 if above else 19)}" font-family="{FONT}" '
                 f'font-size="11.5" fill="{c}" text-anchor="middle" '
                 f'font-weight="{600 if accent else 400}">{esc(label)}</text>')
    return ''.join(o)


def label(x, y, t, size=12, fill=None, weight=400, anchor='start', mono=False):
    return (f'<text x="{x}" y="{y}" font-family="{MONO if mono else FONT}" '
            f'font-size="{size}" fill="{fill or MUTED}" font-weight="{weight}" '
            f'text-anchor="{anchor}">{esc(t)}</text>')


def caption(x, y, t):
    return label(x, y, t, size=11.5, fill=MUTED)


def head(x, y, t):
    return label(x, y, t, size=12.5, fill=INK, weight=700)


def svg(name, w, h, body):
    doc = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}" '
           f'width="{w}" height="{h}" role="img">'
           f'<defs>'
           f'<marker id="am" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" '
           f'markerHeight="6" orient="auto"><path d="M0 0 L10 5 L0 10 z" fill="{MUTED}"/></marker>'
           f'<marker id="ar" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" '
           f'markerHeight="6" orient="auto"><path d="M0 0 L10 5 L0 10 z" fill="{R}"/></marker>'
           f'</defs>'
           f'<rect width="{w}" height="{h}" rx="10" fill="{CREAM}" stroke="{LINE}"/>'
           f'{body}</svg>')
    open(os.path.join(OUT, name), 'w').write(doc)
    print(f'{name:34} {len(doc) / 1024:5.1f} kB  {w}x{h}')


# ---------------------------------------------------------------- 05 the wire
def d05():
    b = [head(28, 34, 'What the backend answers with')]
    b.append(box(28, 56, 150, 62, 'ABAP class', 'builds both', accent=True))
    b.append(arrow(186, 74, 300, 74, 'XML view', accent=True))
    b.append(arrow(186, 100, 300, 100, 'JSON model', accent=True))
    b.append(box(308, 44, 150, 86, 'UI5', 'renders the HTML'))
    b.append(label(28, 152, 'Classic split - the backend sends only the data,'
                            ' the view was deployed with the app', size=11.5))
    b.append(box(28, 166, 150, 44, 'ABAP service', mono=False))
    b.append(arrow(186, 188, 300, 188, 'data'))
    b.append(box(308, 166, 150, 44, 'UI5 + view file'))
    svg('05-two-strings.svg', 486, 232, ''.join(b))


# ------------------------------------------------------- 06 MPA / SPA / HDA
def d06():
    b, x = [head(28, 34, 'Three ways to answer a click')], 28
    cols = [('MPA', 'a whole document,\nevery interaction', False),
            ('SPA', 'routes and state\nin the browser', False),
            ('HDA', 'the server sends\nwhat to show', True)]
    for i, (t, s, acc) in enumerate(cols):
        x = 28 + i * 158
        b.append(box(x, 54, 138, 56, t, accent=acc))
        for j, ln in enumerate(s.split('\n')):
            b.append(label(x + 69, 130 + j * 15, ln, size=11.5, anchor='middle'))
        b.append(label(x + 69, 176, ['full reload', 'rebuild to change', 'nothing to rebuild'][i],
                       size=11.5, fill=R2 if acc else MUTED,
                       weight=600 if acc else 400, anchor='middle'))
    b.append(f'<line x1="28" y1="192" x2="472" y2="192" stroke="{LINE}"/>')
    b.append(caption(28, 212, 'abap2UI5 puts the browser in the third column '
                              '- the sweet spot the other two leave open'))
    svg('06-mpa-spa-hda.svg', 500, 232, ''.join(b))


# ------------------------------------------------------ 07 one generic service
def d07():
    b = [head(28, 34, 'A service per app')]
    for i in range(3):
        y = 52 + i * 46
        b.append(box(28, y, 96, 34, f'app {i + 1}'))
        b.append(arrow(130, y + 17, 168, y + 17))
        b.append(box(174, y, 128, 34, 'own service', dashed=True))
    b.append(caption(28, 206, 'CDS, service definition and binding,'))
    b.append(caption(28, 222, 'behavior definition - per screen'))
    b.append(f'<line x1="330" y1="24" x2="330" y2="236" stroke="{LINE}"/>')
    b.append(head(356, 34, 'One service for all'))
    for i in range(3):
        y = 52 + i * 46
        b.append(box(356, y, 96, 34, f'app {i + 1}'))
        b.append(arrow(458, y + 17, 502, y + 17, accent=True))
    b.append(f'<rect x="508" y="52" width="164" height="126" rx="7" fill="{RL}" '
             f'stroke="{R}" stroke-width="1.5"/>')
    b.append(label(590, 104, 'one generic', size=13.5, fill=R2, weight=600, anchor='middle'))
    b.append(label(590, 122, 'HTTP handler', size=13.5, fill=R2, weight=600, anchor='middle'))
    b.append(label(590, 142, 'two strings', size=11.5, anchor='middle'))
    b.append(caption(356, 206, 'no SEGW project, no service artefact'))
    b.append(caption(356, 222, 'per screen'))
    svg('07-one-service.svg', 700, 250, ''.join(b))


# ------------------------------------------------------------ 08 partial update
def d08():
    b = [head(28, 34, 'Response carries the view')]
    b.append(box(28, 52, 210, 40, 'view + model', accent=False))
    b.append(arrow(133, 96, 133, 124))
    b.append(box(28, 130, 210, 62, 'every control rebuilt',
                 'focus, scroll and typing lost'))
    b.append(f'<line x1="266" y1="24" x2="266" y2="200" stroke="{LINE}"/>')
    b.append(head(292, 34, 'Response carries the model alone'))
    b.append(box(292, 52, 210, 40, 'model', accent=True))
    b.append(arrow(397, 96, 397, 124, accent=True))
    b.append(box(292, 130, 210, 62, 'only bound controls update',
                 'the DOM around them stands', accent=True))
    b.append(caption(28, 218, 'One IF in the app decides which of the two it is'))
    svg('08-partial-update.svg', 530, 238, ''.join(b))


# -------------------------------------------------------------------- 09 draft
def d09():
    b = [head(28, 34, 'One roundtrip')]
    b.append(box(28, 58, 118, 48, 'browser', 'event + input'))
    b.append(arrow(154, 82, 214, 82, 'POST'))
    b.append(box(220, 58, 132, 48, 'new instance', 'of the app class'))
    b.append(f'<line x1="286" y1="114" x2="286" y2="148" stroke="{R}" stroke-width="1.6" '
             f'marker-end="url(#ar)"/>')
    b.append(label(298, 136, 'PUBLIC only', size=11.5, fill=R, weight=600))
    b.append(box(204, 154, 164, 44, 'z2ui5_t_01', accent=True, mono=True))
    b.append(arrow(368, 82, 460, 82, 'view + model'))
    b.append(box(466, 58, 118, 48, 'browser', 'renders'))
    b.append(f'<path d="M368 176 L424 176 L424 90" fill="none" stroke="{R}" '
             f'stroke-width="1.6" stroke-dasharray="5 4"/>')
    b.append(caption(28, 224, 'One generic table for every app - read back on the next '
                              'request, so any server can answer it'))
    svg('09-draft.svg', 612, 244, ''.join(b))


# --------------------------------------------------------------- 17 bootstrap
def d17():
    b = [head(28, 34, 'UI5 served by the system')]
    b.append(box(28, 54, 200, 44, 'SAP release'))
    b.append(arrow(128, 102, 128, 128))
    b.append(box(28, 134, 200, 44, 'the UI5 version you get'))
    b.append(caption(28, 200, 'one moves only when the other does'))
    b.append(f'<line x1="256" y1="24" x2="256" y2="212" stroke="{LINE}"/>')
    b.append(head(282, 34, 'UI5 from a CDN'))
    b.append(box(282, 54, 200, 44, 'SAP release'))
    b.append(box(282, 134, 200, 44, 'the UI5 version you choose', accent=True))
    b.append(f'<line x1="382" y1="102" x2="382" y2="128" stroke="{LINE}" '
             f'stroke-dasharray="4 4"/>')
    b.append(label(392, 120, 'independent', size=11.5, fill=R2, weight=600))
    b.append(caption(282, 200, 'a current control on a system that never had it'))
    svg('17-bootstrap.svg', 540, 226, ''.join(b))


# --------------------------------------------------------------- 18 footprint
def d18():
    b = [head(28, 34, 'Everything in the request path')]
    items = [('1', 'HTTP handler class', True), ('2', 'interfaces', False),
             ('1', 'database table', False)]
    for i, (n, t, acc) in enumerate(items):
        x = 28 + i * 168
        b.append(box(x, 58, 148, 74, n, t, accent=acc))
    b.append(caption(28, 158, 'Originally about 2,300 lines of ABAP. Views, program flow '
                              'and controls live in the apps, not in here.'))
    svg('18-footprint.svg', 552, 178, ''.join(b))


# ---------------------------------------------------------------- 21 timeline
def d21():
    b = [head(28, 32, 'Where the HTML is built')]
    yrs = [('2000', 'ITS'), ('2001', 'BSP'), ('2003', 'Web Dynpro'),
           ('2010', 'UI5 Freestyle'), ('2019', 'RAP / FE'), ('2023', 'abap2UI5')]
    x0, step = 128, 104
    xe = x0 + step * 5
    b.append(f'<line x1="{x0 - 26}" y1="112" x2="{xe + 30}" y2="112" stroke="{LINE}"/>')
    for i, (y, t) in enumerate(yrs):
        x = x0 + i * step
        server = i < 3
        cy = 76 if server else 148
        b.append(f'<circle cx="{x}" cy="112" r="4.5" fill="{R if i == 5 else MUTED}"/>')
        b.append(f'<line x1="{x}" y1="112" x2="{x}" y2="{cy + (16 if server else -16)}" '
                 f'stroke="{LINE}"/>')
        b.append(label(x, cy, t, size=12, fill=R2 if i == 5 else INK,
                       weight=700 if i == 5 else 500, anchor='middle'))
        b.append(label(x, cy + (-15 if server else 16), y, size=11, anchor='middle'))
    b.append(label(28, 66, 'built on', size=11.5, fill=MUTED, weight=600))
    b.append(label(28, 82, 'the server', size=11.5, fill=MUTED, weight=600))
    b.append(label(28, 148, 'built in', size=11.5, fill=MUTED, weight=600))
    b.append(label(28, 164, 'the browser', size=11.5, fill=MUTED, weight=600))
    b.append(caption(28, 214, 'The rendering moved once and stayed. What has been coming '
                              'back since is the definition of the screen.'))
    svg('21-timeline.svg', 720, 234, ''.join(b))


# ------------------------------------------------------- 22 where the view lives
def d22():
    b = [head(28, 34, 'When the view is fixed')]
    cols = [('UI5 Freestyle', 'build time', 'a file in a\nfrontend project', False),
            ('RAP / Fiori Elements', 'activation time', 'UI annotations\non CDS', False),
            ('abap2UI5', 'request time', 'a string from\nan ABAP class', True)]
    for i, (t, when, what, acc) in enumerate(cols):
        x = 28 + i * 196
        b.append(box(x, 56, 176, 46, t, accent=acc))
        b.append(label(x + 88, 126, when, size=12.5, fill=R2 if acc else INK,
                       weight=700, anchor='middle'))
        for j, ln in enumerate(what.split('\n')):
            b.append(label(x + 88, 148 + j * 15, ln, size=11.5, anchor='middle'))
    b.append(f'<line x1="28" y1="196" x2="596" y2="196" stroke="{LINE}"/>')
    b.append(caption(28, 218, 'All three render in the browser, with the same controls. '
                              'This is the axis the rest follows from.'))
    svg('22-where-the-view-lives.svg', 624, 240, ''.join(b))


# ------------------------------------------------------------- 10 view swap
def d10():
    b = [head(28, 34, 'One class, one IF, two views')]
    # left: table
    b.append(box(28, 56, 196, 130, '', ''))
    b.append(label(126, 78, 'as_list = abap_false', size=11.5, mono=True, anchor='middle'))
    for r in range(4):
        y = 96 + r * 20
        for c in range(3):
            x = 46 + c * 60
            b.append(f'<rect x="{x}" y="{y}" width="52" height="13" rx="2" '
                     f'fill="{SAND if r == 0 else WHITE}" stroke="{LINE}"/>')
    b.append(label(126, 200, 'sap.m.Table', size=11.5, anchor='middle'))
    # the switch
    b.append(arrow(232, 120, 276, 120, accent=True))
    b.append(label(254, 146, 'IF', size=12.5, fill=R2, weight=700, anchor='middle', mono=True))
    # right: list
    b.append(box(284, 56, 196, 130, '', ''))
    b.append(label(382, 78, 'as_list = abap_true', size=11.5, mono=True, anchor='middle'))
    for r in range(4):
        y = 94 + r * 22
        b.append(f'<rect x="302" y="{y}" width="160" height="16" rx="2" '
                 f'fill="{WHITE}" stroke="{LINE}"/>')
        b.append(f'<circle cx="312" cy="{y + 8}" r="3" fill="{MUTED}"/>')
    b.append(label(382, 200, 'sap.m.List', size=11.5, anchor='middle'))
    b.append(caption(28, 226, 'Same data, same request handler - a different control, '
                              'chosen in ABAP'))
    svg('10-view-swap.svg', 508, 246, ''.join(b))


# -------------------------------------------------------- 11 initial request
def d11():
    b = [head(28, 34, 'The first GET')]
    b.append(label(28, 56, 'with a BSP', size=12, fill=INK, weight=600))
    for i, t in enumerate(['build', 'deploy', 'transport', 'clear cache']):
        x = 28 + i * 122
        b.append(box(x, 68, 104, 34, t, dashed=True))
        if i < 3:
            b.append(arrow(136 + i * 122, 85, 148 + i * 122, 85))
    b.append(label(28, 136, 'in abap2UI5', size=12, fill=R2, weight=600))
    b.append(box(28, 148, 216, 42, 'a method returns a string', accent=True))
    b.append(arrow(250, 169, 300, 169, 'GET', accent=True))
    b.append(box(306, 148, 158, 42, 'the browser has the app'))
    b.append(caption(28, 216, 'Nothing is built, so nothing can go stale - and every '
                              'file that reaches the browser is in the repository'))
    svg('11-initial-request.svg', 556, 236, ''.join(b))


# ------------------------------------------------------------- 13 four verbs
def d13():
    b = [head(28, 34, 'What each verb does to the cursor')]
    rows = [('ele', 'adds a child and descends into it', 'down'),
            ('tag', 'adds a child and stays', 'stay'),
            ('a', 'sets an attribute on the current element', 'here'),
            ('end', 'ascends to the parent', 'up')]
    for i, (v, t, m) in enumerate(rows):
        y = 58 + i * 38
        b.append(f'<rect x="28" y="{y}" width="62" height="28" rx="5" fill="{RL}" '
                 f'stroke="{R}" stroke-width="1.4"/>')
        b.append(label(59, y + 19, v, size=13, fill=R2, weight=700, anchor='middle', mono=True))
        b.append(label(104, y + 19, t, size=12.5, fill=INK))
        b.append(label(452, y + 19, m, size=11.5, fill=MUTED, anchor='end'))
    b.append(f'<line x1="28" y1="216" x2="452" y2="216" stroke="{LINE}"/>')
    b.append(caption(28, 236, 'None of the four names a control, which is why every '
                              'control is reachable'))
    svg('13-four-verbs.svg', 480, 256, ''.join(b))


# --------------------------------------------------------------------- 20 VDM
def d20():
    b = [head(28, 34, 'What the app reads')]
    b.append(box(28, 56, 180, 38, 'your app'))
    b.append(arrow(118, 98, 118, 124, accent=True))
    b.append(box(28, 130, 180, 38, 'released CDS view', accent=True))
    b.append(arrow(118, 172, 118, 196))
    b.append(box(28, 202, 180, 38, 'tables', dashed=True))
    b.append(label(118, 262, 'the shape SAP maintains', size=11.5, fill=R2,
                   weight=600, anchor='middle'))
    b.append(f'<line x1="248" y1="24" x2="248" y2="278" stroke="{LINE}"/>')
    b.append(box(274, 56, 180, 38, 'your app'))
    b.append(f'<path d="M364 98 L364 196" fill="none" stroke="{MUTED}" '
             f'stroke-width="1.6" marker-end="url(#am)"/>')
    b.append(box(274, 202, 180, 38, 'tables', dashed=True))
    b.append(label(376, 149, 'nothing in between', size=11.5))
    b.append(label(364, 262, 'the shape of this release', size=11.5, anchor='middle'))
    svg('20-vdm.svg', 482, 292, ''.join(b))


for f in (d05, d06, d07, d08, d09, d10, d11, d13, d17, d18, d20, d21, d22):
    f()
