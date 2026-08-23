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
    return base64.b64encode(open(M+p,'rb').read()).decode()

dino, sheep, sloth = b64('dinosaur_brand.png'), b64('sheep_brand.png'), b64('sloth_brand.png')

RED, DARKRED = '#D03C4A', '#A83232'
INK, MUTED   = '#2A2A2E', '#7A7A82'
W, H = 1200, 630

COLS = ['CARRID', 'CONNID', 'FLDATE', 'PRICE']
ROWS = [['LH','0400','2026-08-25','899.00'],
        ['LH','0402','2026-08-26','915.00'],
        ['AA','0017','2026-08-27','422.50'],
        ['UA','0941','2026-08-28','780.00']]

o = []
a = o.append

a(f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
  f'width="{W}" height="{H}" viewBox="0 0 {W} {H}" font-family="Helvetica Neue,Helvetica,Arial,sans-serif">')

a('''<defs>
<filter id="sh" x="-20%" y="-20%" width="140%" height="150%">
  <feDropShadow dx="0" dy="6" stdDeviation="9" flood-color="#1A1A22" flood-opacity="0.13"/>
</filter>
<filter id="shs" x="-20%" y="-20%" width="140%" height="150%">
  <feDropShadow dx="0" dy="2" stdDeviation="3" flood-color="#1A1A22" flood-opacity="0.10"/>
</filter>
<marker id="ah" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">
  <path d="M0,0 L10,5 L0,10 z" fill="#D03C4A"/>
</marker>
</defs>''')

# ---------- background ----------
a(f'<rect width="{W}" height="{H}" fill="#FBFAFA"/>')
a(f'<rect x="0" y="0" width="{W}" height="7" fill="{RED}"/>')

# ---------- headline ----------
a(f'<text x="64" y="86" font-size="39" font-weight="700" fill="{INK}" '
  f'letter-spacing="-0.6">Somewhere on the way to UI5, we lost RTTI</text>')
a(f'<text x="64" y="118" font-size="18" fill="{MUTED}">'
  f'Screens for tables nobody had seen — and the road back</text>')

# ---------- table renderer ----------
def alv(x, y, w, h):
    """classic ALV grid: grey chrome, tight rows, blue-grey header"""
    g = []
    g.append(f'<g filter="url(#sh)"><rect x="{x}" y="{y}" width="{w}" height="{h}" '
             f'fill="#F2F2F0" stroke="#9A9A96" stroke-width="1"/></g>')
    # title bar
    g.append(f'<rect x="{x+1}" y="{y+1}" width="{w-2}" height="21" fill="#E4E3DE"/>')
    g.append(f'<text x="{x+9}" y="{y+16}" font-size="11" fill="#4A4A48" '
             f'font-family="Arial,sans-serif">Display Table</text>')
    # toolbar buttons
    bx = x + 8
    for i in range(7):
        g.append(f'<rect x="{bx}" y="{y+28}" width="17" height="15" fill="#DCDBD6" stroke="#A9A8A2"/>')
        bx += 20
        if i in (2, 4):
            bx += 7
    ty = y + 51
    cw = (w - 16) / len(COLS)
    # header row
    g.append(f'<rect x="{x+8}" y="{ty}" width="{w-16}" height="20" fill="#C6D2DC" stroke="#8E9AA4"/>')
    for i, c in enumerate(COLS):
        cx = x + 8 + i * cw
        if i:
            g.append(f'<line x1="{cx}" y1="{ty}" x2="{cx}" y2="{ty+20}" stroke="#8E9AA4"/>')
        g.append(f'<text x="{cx+7}" y="{ty+14}" font-size="10.5" font-weight="bold" fill="#25313B" '
                 f'font-family="Arial,sans-serif">{c}</text>')
    # data rows
    for r, row in enumerate(ROWS):
        ry = ty + 20 + r * 19
        fill = '#FFFFFF' if r % 2 == 0 else '#F4F6F8'
        g.append(f'<rect x="{x+8}" y="{ry}" width="{w-16}" height="19" fill="{fill}" stroke="#C8CCD0"/>')
        for i, v in enumerate(row):
            cx = x + 8 + i * cw
            if i:
                g.append(f'<line x1="{cx}" y1="{ry}" x2="{cx}" y2="{ry+19}" stroke="#C8CCD0"/>')
            anc = 'end' if i == 3 else 'start'
            tx = cx + cw - 7 if i == 3 else cx + 7
            g.append(f'<text x="{tx}" y="{ry+13}" font-size="10.5" fill="#33333A" text-anchor="{anc}" '
                     f'font-family="Arial,sans-serif">{v}</text>')
    # status strip, so this frame matches the UI5 card in height
    g.append(f'<rect x="{x+1}" y="{y+h-19}" width="{w-2}" height="18" fill="#E4E3DE"/>')
    g.append(f'<text x="{x+9}" y="{y+h-6}" font-size="10" fill="#6A6A66" '
             f'font-family="Arial,sans-serif">{len(ROWS)} entries</text>')
    return '\n'.join(g)

def ui5(x, y, w, h):
    """sap.m.Table: white card, generous rows, red accent"""
    g = []
    g.append(f'<g filter="url(#sh)"><rect x="{x}" y="{y}" width="{w}" height="{h}" rx="9" '
             f'fill="#FFFFFF" stroke="#E3E3E6"/></g>')
    g.append(f'<text x="{x+20}" y="{y+31}" font-size="15" font-weight="600" fill="{INK}">Flights</text>')
    g.append(f'<rect x="{x+w-64}" y="{y+17}" width="44" height="18" rx="9" fill="{RED}" opacity="0.12"/>')
    g.append(f'<text x="{x+w-42}" y="{y+30}" font-size="10" font-weight="600" fill="{DARKRED}" '
             f'text-anchor="middle">RTTI</text>')
    ty = y + 46
    cw = (w - 40) / len(COLS)
    g.append(f'<line x1="{x+20}" y1="{ty+21}" x2="{x+w-20}" y2="{ty+21}" stroke="#E3E3E6"/>')
    for i, c in enumerate(COLS):
        cx = x + 20 + i * cw
        anc = 'end' if i == 3 else 'start'
        tx = cx + cw - 4 if i == 3 else cx
        g.append(f'<text x="{tx}" y="{ty+15}" font-size="11" font-weight="600" fill="{MUTED}" '
                 f'text-anchor="{anc}" letter-spacing="0.4">{c}</text>')
    for r, row in enumerate(ROWS):
        ry = ty + 21 + r * 27
        if r:
            g.append(f'<line x1="{x+20}" y1="{ry}" x2="{x+w-20}" y2="{ry}" stroke="#F0F0F2"/>')
        for i, v in enumerate(row):
            cx = x + 20 + i * cw
            anc = 'end' if i == 3 else 'start'
            tx = cx + cw - 4 if i == 3 else cx
            wt = '600' if i == 0 else '400'
            g.append(f'<text x="{tx}" y="{ry+18}" font-size="12.5" fill="{INK}" font-weight="{wt}" '
                     f'text-anchor="{anc}">{v}</text>')
    return '\n'.join(g)

TY, TH = 134, 190
a(alv(64, TY, 412, TH))
a(ui5(724, TY, 412, TH))

# ---------- the bridge ----------
cx0, cx1, my = 500, 700, TY + TH / 2
a(f'<line x1="{cx0}" y1="{my}" x2="{cx1-10}" y2="{my}" stroke="{RED}" stroke-width="2.5" '
  f'marker-end="url(#ah)"/>')
a(f'<text x="{(cx0+cx1)/2}" y="{my-38}" font-size="12.5" fill="{INK}" text-anchor="middle" '
  f'font-family="Menlo,Consolas,monospace">cl_abap_structdescr</text>')
a(f'<text x="{(cx0+cx1)/2}" y="{my-19}" font-size="12.5" fill="{INK}" text-anchor="middle" '
  f'font-family="Menlo,Consolas,monospace">-&gt;get_components( )</text>')
a(f'<text x="{(cx0+cx1)/2}" y="{my+30}" font-size="12" fill="{MUTED}" text-anchor="middle" '
  f'font-style="italic">columns discovered,</text>')
a(f'<text x="{(cx0+cx1)/2}" y="{my+47}" font-size="12" fill="{MUTED}" text-anchor="middle" '
  f'font-style="italic">not declared</text>')

# ---------- mascots ----------
def img(data, x, y, s):
    return (f'<image xlink:href="data:image/png;base64,{data}" x="{x}" y="{y}" '
            f'width="{s}" height="{s}"/>')

FLOOR = 500
# a ground band, so the animals stand on something instead of floating
a(f'<rect x="0" y="{FLOOR}" width="{W}" height="{H-FLOOR}" fill="#F1EFEF"/>')
a(f'<line x1="0" y1="{FLOOR}" x2="{W}" y2="{FLOOR}" stroke="#E2DFDF"/>')
for cx_, rx_ in ((271, 96), (933, 128)):
    a(f'<ellipse cx="{cx_}" cy="{FLOOR}" rx="{rx_}" ry="9" fill="#1A1A22" opacity="0.07"/>')
a(img(dino,  188, FLOOR-166, 166))   # the old hand, at the ALV
a(img(sheep, 772, FLOOR-152, 152))   # the enthusiast
a(img(sloth, 928, FLOOR-166, 166))   # at the keyboard

# labels under each side
a(f'<text x="271" y="{FLOOR+30}" font-size="14" font-weight="700" fill="#5E5E66" '
  f'text-anchor="middle">ALV, since forever</text>')
a(f'<text x="933" y="{FLOOR+30}" font-size="14" font-weight="700" fill="#5E5E66" '
  f'text-anchor="middle">the same table, in UI5</text>')

# ---------- footer, inside the ground band ----------
a(f'<text x="64" y="{H-22}" font-size="13" fill="#8A8A90">abap2UI5 Know-How · #1</text>')
a(f'<text x="{W-64}" y="{H-22}" font-size="13" font-weight="700" fill="{RED}" text-anchor="end">'
  f'abap2UI5.org</text>')

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
