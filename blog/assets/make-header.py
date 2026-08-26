#!/usr/bin/env python3
"""Header image for blog article #1 — the mascots, an ALV grid and a UI5 table.

Run from the repository root:  python3 blog/assets/make-header.py

Writes blog/assets/01-rtti-header.png at 2400x1260 (a 1200x630 design at 2x,
the size LinkedIn wants for a link preview).

The three mascots are the project's own brand assets from docs/public/mascots/,
embedded as-is. They are NOT redrawn - docs/resources/logo.md is explicit that
brand art is scaled and nothing else, and a hand-traced dinosaur would be both
off-brand and worse than the original.

Everything else - the ALV grid and the UI5 table - is drawn here, because the
point of the picture is that the two are the same data behind different faces.

Rendering goes through the Chromium that ships with this environment. It is
told to lay out a page TALLER than the design and the result is cropped down,
because --screenshot clips a page to less than its --window-size height and
would otherwise cut the footer off.
"""
import base64, os, shutil, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
M = os.path.join(ROOT, 'docs', 'public', 'mascots') + os.sep
OUT = os.path.join(ROOT, 'blog', 'assets', '01-rtti-header.png')
CHROME = '/opt/pw-browsers/chromium-1194/chrome-linux/chrome'


def b64(p):
    return base64.b64encode(open(M + p, 'rb').read()).decode()


dino, sheep, sloth = b64('dinosaur_brand.png'), b64('sheep_brand.png'), b64('sloth_brand.png')

RED, DARKRED = '#D03C4A', '#A83232'
INK, MUTED = '#2A2A2E', '#7A7A82'
W, H = 1200, 630

o = []
a = o.append

a(f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
  f'width="{W}" height="{H}" viewBox="0 0 {W} {H}" '
  f'font-family="Helvetica Neue,Helvetica,Arial,sans-serif">')

a('''<defs>
<filter id="sh" x="-20%" y="-20%" width="140%" height="150%">
  <feDropShadow dx="0" dy="6" stdDeviation="9" flood-color="#1A1A22" flood-opacity="0.13"/>
</filter>
<marker id="ah" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">
  <path d="M0,0 L10,5 L0,10 z" fill="#B9B6B6"/>
</marker>
<marker id="ahr" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">
  <path d="M0,0 L10,5 L0,10 z" fill="#D03C4A"/>
</marker>
</defs>''')

a(f'<rect width="{W}" height="{H}" fill="#FBFAFA"/>')
a(f'<rect x="0" y="0" width="{W}" height="7" fill="{RED}"/>')

a(f'<text x="64" y="86" font-size="39" font-weight="700" fill="{INK}" letter-spacing="-0.6">'
  f'Somewhere on the way to UI5, we lost RTTI</text>')
a(f'<text x="64" y="118" font-size="18" fill="{MUTED}">'
  f'Two roads to the same table — and only one of them waits for a type</text>')


def chip(x, y, w, text, accent=False):
    """one step of a lane"""
    fill, stroke = ('#FDF0F1', RED) if accent else ('#FFFFFF', '#DAD8D8')
    col = DARKRED if accent else '#4A4A52'
    wt = '700' if accent else '500'
    return (f'<g><rect x="{x}" y="{y}" width="{w}" height="38" rx="7" fill="{fill}" '
            f'stroke="{stroke}" stroke-width="{1.6 if accent else 1}"/>'
            f'<text x="{x + w / 2}" y="{y + 24}" font-size="13" font-weight="{wt}" fill="{col}" '
            f'text-anchor="middle" font-family="Menlo,Consolas,monospace">{text}</text></g>')


def lane(y, label, note, steps, accent=False):
    # one text element with two tspans - the browser lays the gap out, which a
    # width estimate here got wrong and overlapped the note onto the label
    g = [f'<text x="64" y="{y - 14}" font-size="13.5">'
         f'<tspan font-weight="700" fill="{RED if accent else MUTED}" '
         f'letter-spacing="0.6">{label.upper()}</tspan>'
         f'<tspan font-size="13" fill="#A5A2A2" letter-spacing="0"> &#160; {note}</tspan></text>']
    x = 64
    for i, (txt, w) in enumerate(steps):
        if i:
            g.append(f'<line x1="{x - 20}" y1="{y + 19}" x2="{x - 7}" y2="{y + 19}" '
                     f'stroke="#B9B6B6" stroke-width="1.6" marker-end="url(#ah)"/>')
        g.append(chip(x, y, w, txt, accent and i == len(steps) - 1))
        x += w + 20
    # into the shared screen
    g.append(f'<line x1="{x - 20}" y1="{y + 19}" x2="716" y2="{y + 19}" '
             f'stroke="{RED if accent else "#B9B6B6"}" stroke-width="{2.2 if accent else 1.6}" '
             f'marker-end="url(#{"ahr" if accent else "ah"})"/>')
    return '\n'.join(g)


a(lane(168, 'design time', 'the model is decided before the code runs',
       [('CDS view', 108), ('entity type', 122), ('OData metadata', 148)]))
a(lane(268, 'runtime', 'the model is whatever the data turns out to be',
       [('internal table', 140), ('RTTS', 84)], accent=True))

# the shared destination
CX, CY, CW, CH = 736, 132, 400, 204
a(f'<g filter="url(#sh)"><rect x="{CX}" y="{CY}" width="{CW}" height="{CH}" rx="9" '
  f'fill="#FFFFFF" stroke="#E3E3E6"/></g>')
a(f'<text x="{CX + 20}" y="{CY + 31}" font-size="15" font-weight="600" fill="{INK}">'
  f'the same sap.m.Table</text>')
COLS = ['CARRID', 'CONNID', 'FLDATE', 'PRICE']
ROWS = [['LH', '0400', '2026-08-25', '899.00'],
        ['LH', '0402', '2026-08-26', '915.00'],
        ['AA', '0017', '2026-08-27', '422.50'],
        ['UA', '0941', '2026-08-28', '780.00']]
ty = CY + 46
cw = (CW - 40) / len(COLS)
a(f'<line x1="{CX + 20}" y1="{ty + 21}" x2="{CX + CW - 20}" y2="{ty + 21}" stroke="#E3E3E6"/>')
for i, c in enumerate(COLS):
    cx = CX + 20 + i * cw
    anc = 'end' if i == 3 else 'start'
    tx = cx + cw - 4 if i == 3 else cx
    a(f'<text x="{tx}" y="{ty + 15}" font-size="11" font-weight="600" fill="{MUTED}" '
      f'text-anchor="{anc}" letter-spacing="0.4">{c}</text>')
for r, row in enumerate(ROWS):
    ry = ty + 21 + r * 30
    if r:
        a(f'<line x1="{CX + 20}" y1="{ry}" x2="{CX + CW - 20}" y2="{ry}" stroke="#F0F0F2"/>')
    for i, v in enumerate(row):
        cx = CX + 20 + i * cw
        anc = 'end' if i == 3 else 'start'
        tx = cx + cw - 4 if i == 3 else cx
        a(f'<text x="{tx}" y="{ry + 20}" font-size="12.5" fill="{INK}" '
          f'font-weight="{"600" if i == 0 else "400"}" text-anchor="{anc}">{v}</text>')

# ground band and the mascots on it
FLOOR = 500
a(f'<rect x="0" y="{FLOOR}" width="{W}" height="{H - FLOOR}" fill="#F1EFEF"/>')
a(f'<line x1="0" y1="{FLOOR}" x2="{W}" y2="{FLOOR}" stroke="#E2DFDF"/>')
for cx_, rx_ in ((150, 78), (300, 72), (450, 78)):
    a(f'<ellipse cx="{cx_}" cy="{FLOOR}" rx="{rx_}" ry="8" fill="#1A1A22" opacity="0.07"/>')


def img(data, x, y, s):
    return (f'<image xlink:href="data:image/png;base64,{data}" x="{x}" y="{y}" '
            f'width="{s}" height="{s}"/>')


a(img(dino, 78, FLOOR - 144, 150))
a(img(sheep, 236, FLOOR - 130, 136))
a(img(sloth, 378, FLOOR - 144, 150))

# the caption sits in the empty band between the card and the ground, not
# stacked on the footer - three right-aligned lines in a row read as one block
a(f'<text x="{W - 64}" y="424" font-size="21" font-weight="700" fill="#4A4A52" '
  f'text-anchor="end">RTTS never went away.</text>')
a(f'<text x="{W - 64}" y="454" font-size="21" font-weight="700" fill="{RED}" '
  f'text-anchor="end">Only the screen in front of it did.</text>')

a(f'<text x="64" y="{H - 22}" font-size="13" fill="#8A8A90">abap2UI5 Know-How \u00b7 #1</text>')
a(f'<text x="{W - 64}" y="{H - 22}" font-size="13" font-weight="700" fill="{RED}" '
  f'text-anchor="end">abap2UI5.org</text>')

a('</svg>')

svg = '\n'.join(o)

if not os.path.exists(CHROME):
    sys.exit(f'chromium not found at {CHROME} - adjust CHROME and re-run')

with tempfile.TemporaryDirectory() as tmp:
    page = os.path.join(tmp, 'page.html')
    with open(page, 'w') as f:
        f.write('<!doctype html><meta charset=utf-8>'
                '<style>html,body{margin:0;padding:0;overflow:hidden;background:#FBFAFA}'
                'svg{display:block}</style>')
        f.write(svg)
    raw = os.path.join(tmp, 'raw.png')
    subprocess.run([CHROME, '--headless', '--disable-gpu', '--no-sandbox',
                    '--hide-scrollbars', '--force-device-scale-factor=2',
                    f'--window-size={W},{H + 230}',      # taller: see the module docstring
                    f'--screenshot={raw}', 'file://' + page],
                   check=True, capture_output=True)
    try:
        from PIL import Image
    except ImportError:
        sys.exit('needs pillow for the crop:  pip install pillow')
    Image.open(raw).convert('RGB').crop((0, 0, W * 2, H * 2)).save(OUT, optimize=True)

print(f'wrote {OUT}  ({os.path.getsize(OUT) // 1024} KB)')
