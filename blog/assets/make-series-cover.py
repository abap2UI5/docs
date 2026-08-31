#!/usr/bin/env python3
"""The reusable cover for the abap2UI5 Know-How series - a reading scene.

Run from the repository root:  python3 blog/assets/make-series-cover.py
Writes blog/assets/series-cover.png at 2400x1260 (a 1200x630 design at 2x).

Almost no words on purpose. The picture is the series: the dinosaur deep in a
book with a thought over its head, the sheep reading beside it, the sloth
already at the keyboard - learn, think, build. That reads for any article,
which is what a series cover has to do.

The three mascots are the project's brand art, embedded from docs/public/ and
only scaled; docs/resources/logo.md is explicit that they are never redrawn.
Everything around them - the shelf, the stacks, the open book, the desk, the
lamp - is drawn here.

Rendering goes through the Chromium that ships with this environment, laying
out a page taller than the design and cropping it, because --screenshot clips a
page to less than its --window-size height.
"""
import base64, os, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PUB = os.path.join(ROOT, 'docs', 'public')
OUT = os.path.join(ROOT, 'blog', 'assets', 'series-cover.png')
CHROME = '/opt/pw-browsers/chromium-1194/chrome-linux/chrome'

b64 = lambda p: base64.b64encode(open(os.path.join(PUB, p), 'rb').read()).decode()
logo, dino, sheep, sloth = (b64('logo.png'), b64('mascots/dinosaur_brand.png'),
                            b64('mascots/sheep_brand.png'), b64('mascots/sloth_brand.png'))

RED, DARK = '#D03C4A', '#A83232'
INK, MUTED = '#2A2328', '#8B8087'
NAVY, TAN, CREAM, SLATE = '#33405A', '#C8A480', '#EADDCB', '#7E8B99'
W, H = 1200, 630
FLOOR = 486

o = []
a = o.append
a(f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
  f'width="{W}" height="{H}" viewBox="0 0 {W} {H}" '
  f'font-family="Helvetica Neue,Helvetica,Arial,sans-serif">')

a(f'''<defs>
<linearGradient id="bg" x1="0" y1="0" x2="0.4" y2="1">
  <stop offset="0" stop-color="#FDF7F3"/><stop offset="1" stop-color="#F6EBE4"/>
</linearGradient>
<radialGradient id="lamp" cx="0.5" cy="0.5" r="0.5">
  <stop offset="0" stop-color="#FFE9B8" stop-opacity="0.85"/>
  <stop offset="1" stop-color="#FFE9B8" stop-opacity="0"/>
</radialGradient>
<linearGradient id="floor" x1="0" y1="0" x2="0" y2="1">
  <stop offset="0" stop-color="#E9DACE"/><stop offset="1" stop-color="#E2D0C1"/>
</linearGradient>
</defs>''')

a(f'<rect width="{W}" height="{H}" fill="url(#bg)"/>')


def book_flat(x, y, w, col, h=15):
    """one volume lying down, seen from the side"""
    return (f'<g><rect x="{x}" y="{y}" width="{w}" height="{h}" rx="3" fill="{col}"/>'
            f'<rect x="{x + 5}" y="{y + 3}" width="{w - 10}" height="{h - 6}" rx="1.5" '
            f'fill="#FFFFFF" opacity="0.20"/>'
            f'<rect x="{x + w - 9}" y="{y + 2}" width="4" height="{h - 4}" rx="1.5" '
            f'fill="#000000" opacity="0.10"/></g>')


def book_up(x, y, w, h, col, tilt=0):
    """one volume standing on a shelf"""
    g = (f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="2.5" fill="{col}"/>'
         f'<rect x="{x + 3}" y="{y + 6}" width="{w - 6}" height="3" fill="#FFFFFF" opacity="0.28"/>'
         f'<rect x="{x + 3}" y="{y + h - 14}" width="{w - 6}" height="3" fill="#FFFFFF" '
         f'opacity="0.28"/>')
    if tilt:
        return f'<g transform="rotate({tilt} {x + w / 2} {y + h})">{g}</g>'
    return f'<g>{g}</g>'


# ---- back wall: a shelf, kept faint so it stays scenery ----------------------
a(f'<g opacity="0.5">')
for sx, sy in ((96, 92), (96, 232)):
    a(f'<rect x="{sx}" y="{sy + 104}" width="368" height="9" rx="3" fill="{TAN}"/>')
    bx = sx + 14
    for w, h, col, tilt in ((22, 96, NAVY, 0), (17, 88, RED, 0), (25, 100, TAN, 0),
                            (15, 82, SLATE, 0), (20, 92, CREAM, 0), (18, 78, DARK, -9),
                            (24, 98, NAVY, 0), (16, 86, TAN, 0), (21, 90, SLATE, 0),
                            (19, 94, RED, 0), (23, 84, CREAM, 0), (17, 96, NAVY, -7)):
        a(book_up(bx, sy + 104 - h, w, h, col, tilt))
        bx += w + 5
a('</g>')

# ---- the lamp, and the glow it casts over the reading corner ---------------
a(f'<ellipse cx="596" cy="300" rx="330" ry="260" fill="url(#lamp)"/>')
a(f'<line x1="596" y1="0" x2="596" y2="86" stroke="#B9A899" stroke-width="3"/>')
a(f'<path d="M 540 150 L 566 90 L 626 90 L 652 150 Z" fill="{DARK}"/>')
a(f'<path d="M 540 150 L 652 150 L 646 158 L 546 158 Z" fill="{RED}"/>')
a(f'<ellipse cx="596" cy="154" rx="52" ry="7" fill="#FFE9B8" opacity="0.9"/>')

# ---- desk -------------------------------------------------------------------
a(f'<rect x="0" y="{FLOOR}" width="{W}" height="{H - FLOOR}" fill="url(#floor)"/>')
a(f'<line x1="0" y1="{FLOOR}" x2="{W}" y2="{FLOOR}" stroke="#D3BFAE" stroke-width="2"/>')
for cx_, rx_ in ((300, 96), (596, 128), (872, 104)):
    a(f'<ellipse cx="{cx_}" cy="{FLOOR + 2}" rx="{rx_}" ry="11" fill="#8A6B55" opacity="0.13"/>')

# ---- left: a stack of volumes, the sheep reading on top of it ---------------
sx, sy = 196, FLOOR
for w, col, h in ((150, NAVY, 17), (138, TAN, 14), (156, RED, 16), (132, CREAM, 13)):
    sy -= h
    a(book_flat(sx + (156 - w) // 2, sy, w, col, h))
a(f'<image xlink:href="data:image/png;base64,{sheep}" x="206" y="{sy - 118}" '
  f'width="126" height="126"/>')

# ---- centre: the dinosaur, and the open book it is deep in ------------------
a(f'<image xlink:href="data:image/png;base64,{dino}" x="512" y="292" width="188" height="188"/>')

BXc, BYc = 606, 466
a(f'<g transform="rotate(-2 {BXc} {BYc})">'
  f'<path d="M {BXc - 132} {BYc} q 66 -26 132 -6 q 66 -20 132 6 '
  f'l 0 16 q -66 -22 -132 -2 q -66 -20 -132 2 z" fill="#FFFFFF" stroke="#D9CBBD" '
  f'stroke-width="2" stroke-linejoin="round"/>'
  f'<path d="M {BXc} {BYc - 6} l 0 18" stroke="#D9CBBD" stroke-width="2"/>')
for i in range(5):
    yy = BYc - 20 + i * 5
    a(f'<line x1="{BXc - 112}" y1="{yy + 12}" x2="{BXc - 22}" y2="{yy + 9}" '
      f'stroke="#C9BCAE" stroke-width="1.6" opacity="0.75"/>')
    a(f'<line x1="{BXc + 22}" y1="{yy + 9}" x2="{BXc + 112}" y2="{yy + 12}" '
      f'stroke="#C9BCAE" stroke-width="1.6" opacity="0.75"/>')
a(f'<path d="M {BXc - 138} {BYc + 4} q 68 -26 138 -6 q 70 -20 138 6 l 0 10 '
  f'q -68 -24 -138 -4 q -70 -20 -138 4 z" fill="{TAN}" opacity="0.9"/></g>')

# a couple of volumes waiting beside it
a(book_flat(742, FLOOR - 15, 118, SLATE, 15))
a(book_flat(748, FLOOR - 28, 106, RED, 13))

# ---- the thought over the dinosaur's head -----------------------------------
a(f'<circle cx="712" cy="292" r="7" fill="#FFFFFF" stroke="#DDD0C4" stroke-width="2"/>')
a(f'<circle cx="732" cy="268" r="11" fill="#FFFFFF" stroke="#DDD0C4" stroke-width="2"/>')
a(f'<circle cx="794" cy="206" r="52" fill="#FFFFFF" stroke="#DDD0C4" stroke-width="2.5"/>')
a(f'<path d="M 794 178 a 22 22 0 0 1 12 41 l 0 9 a 12 12 0 0 1 -24 0 l 0 -9 '
  f'a 22 22 0 0 1 12 -41 z" fill="{RED}" opacity="0.92"/>')
a(f'<rect x="786" y="230" width="17" height="5" rx="2.5" fill="{DARK}"/>')
a(f'<rect x="788" y="238" width="13" height="4" rx="2" fill="{DARK}"/>')
for ang, r0, r1 in ((-58, 60, 72), (-90, 60, 72), (-122, 60, 72)):
    import math
    rad = math.radians(ang)
    a(f'<line x1="{794 + r0 * math.cos(rad):.1f}" y1="{206 + r0 * math.sin(rad):.1f}" '
      f'x2="{794 + r1 * math.cos(rad):.1f}" y2="{206 + r1 * math.sin(rad):.1f}" '
      f'stroke="{RED}" stroke-width="3" stroke-linecap="round" opacity="0.55"/>')

# ---- right: the sloth, already building ------------------------------------
a(f'<image xlink:href="data:image/png;base64,{sloth}" x="808" y="{FLOOR - 158}" '
  f'width="164" height="164"/>')
a(book_flat(984, FLOOR - 15, 104, NAVY, 15))
a(book_flat(990, FLOOR - 28, 92, TAN, 13))
a(book_flat(996, FLOOR - 39, 80, RED, 11))

# ---- the only words on the picture -----------------------------------------
a(f'<image xlink:href="data:image/png;base64,{logo}" x="62" y="{H - 118}" '
  f'width="76" height="74"/>')
a(f'<text x="152" y="{H - 68}" font-size="30" font-weight="700" letter-spacing="-1.1">'
  f'<tspan fill="{RED}">#</tspan><tspan fill="{INK}">KNOW-HOW</tspan></text>')
a(f'<text x="154" y="{H - 46}" font-size="12" font-weight="700" fill="{MUTED}" '
  f'letter-spacing="2.6">abap2UI5 SERIES</text>')
a(f'<text x="{W - 62}" y="{H - 48}" font-size="14" font-weight="700" fill="{RED}" '
  f'text-anchor="end">abap2UI5.org</text>')

a('</svg>')
svg = '\n'.join(o)

if not os.path.exists(CHROME):
    sys.exit(f'chromium not found at {CHROME} - adjust CHROME and re-run')

with tempfile.TemporaryDirectory() as tmp:
    page = os.path.join(tmp, 'page.html')
    with open(page, 'w') as f:
        f.write('<!doctype html><meta charset=utf-8>'
                '<style>html,body{margin:0;padding:0;overflow:hidden;background:#FDF7F3}'
                'svg{display:block}</style>')
        f.write(svg)
    raw = os.path.join(tmp, 'raw.png')
    subprocess.run([CHROME, '--headless', '--disable-gpu', '--no-sandbox', '--hide-scrollbars',
                    '--force-device-scale-factor=2', f'--window-size={W},{H + 230}',
                    f'--screenshot={raw}', 'file://' + page], check=True, capture_output=True)
    from PIL import Image
    Image.open(raw).convert('RGB').crop((0, 0, W * 2, H * 2)).save(OUT, optimize=True)

print(f'wrote {OUT}  ({os.path.getsize(OUT) // 1024} KB)')
