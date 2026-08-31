#!/usr/bin/env python3
"""The reusable cover for the abap2UI5 Know-How series - one reading scene.

Run from the repository root:  python3 blog/assets/make-series-cover.py
Writes blog/assets/series-cover.png at 2400x1260 (a 1200x630 design at 2x).

Almost no words: the dinosaur deep in a book with a thought over its head, the
sheep reading beside it, the sloth already at the keyboard. Learn, think,
build - which is true of every article, and that is the test a series cover has
to pass.

EVERYTHING IS DRAWN IN ONE LANGUAGE. The first version put flat pastel props
around the mascots and the two never became one picture. So the props now use
the mascots' own vocabulary: three values of the brand red, cream, and details
KNOCKED OUT IN WHITE rather than outlined - which is exactly how the animals
are built. Nothing here has a dark outline, nothing has a gradient, and no prop
introduces a hue the mascots do not already contain.

The mascots themselves are brand art, embedded from docs/public/ and only
scaled. docs/resources/logo.md is explicit that brand art is never redrawn, and
a hand-traced dinosaur would be off-brand as well as worse.
"""
import base64, math, os, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PUB = os.path.join(ROOT, 'docs', 'public')
OUT = os.path.join(ROOT, 'blog', 'assets', 'series-cover.png')
CHROME = '/opt/pw-browsers/chromium-1194/chrome-linux/chrome'

b64 = lambda p: base64.b64encode(open(os.path.join(PUB, p), 'rb').read()).decode()
logo, dino, sheep, sloth = (b64('logo.png'), b64('mascots/dinosaur_brand.png'),
                            b64('mascots/sheep_brand.png'), b64('mascots/sloth_brand.png'))

# the mascots' own palette, and nothing else
R1, R2, R3 = '#D03C4A', '#A83232', '#E79aa1'
CREAM, SAND, WHITE = '#FAF2EC', '#F0DFD2', '#FFFFFF'
INK, MUTED = '#3A2A2E', '#9C8A8E'
W, H = 1200, 630
FLOOR = 492

o = []
a = o.append
a(f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
  f'width="{W}" height="{H}" viewBox="0 0 {W} {H}" '
  f'font-family="Helvetica Neue,Helvetica,Arial,sans-serif">')
a(f'<rect width="{W}" height="{H}" fill="{CREAM}"/>')

# the mark's own circle, enlarged into a halo behind the reader
a(f'<circle cx="600" cy="336" r="238" fill="{R1}" opacity="0.055"/>')


def spine(x, y, w, h, col, tilt=0):
    """a standing volume - two white bands knocked out, the way the animals are"""
    g = (f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="2.5" fill="{col}"/>'
         f'<rect x="{x + 3.5}" y="{y + 8}" width="{w - 7}" height="2.6" fill="{WHITE}" '
         f'opacity="0.72"/>'
         f'<rect x="{x + 3.5}" y="{y + h - 15}" width="{w - 7}" height="2.6" fill="{WHITE}" '
         f'opacity="0.72"/>')
    return (f'<g transform="rotate({tilt} {x + w / 2} {y + h})">{g}</g>' if tilt else f'<g>{g}</g>')


def flat(x, y, w, col, h=15):
    """a volume lying down - one white rule along the fore-edge"""
    return (f'<g><rect x="{x}" y="{y}" width="{w}" height="{h}" rx="3" fill="{col}"/>'
            f'<rect x="{x + 6}" y="{y + h / 2 - 1.3}" width="{w - 20}" height="2.6" rx="1.3" '
            f'fill="{WHITE}" opacity="0.66"/></g>')


# ---- the shelf ---------------------------------------------------------------
SHELF = [(112, 214), (112, 356)]
for sx, sy in SHELF:
    a(f'<rect x="{sx}" y="{sy}" width="356" height="10" rx="5" fill="{R2}"/>')
    a(f'<rect x="{sx + 8}" y="{sy + 3.4}" width="340" height="2.6" rx="1.3" fill="{WHITE}" '
      f'opacity="0.5"/>')
    bx = sx + 12
    for w, h, col, tilt in ((21, 96, R1, 0), (16, 84, SAND, 0), (24, 104, R2, 0),
                            (15, 78, R3, 0), (20, 92, R1, 0), (18, 72, SAND, -10),
                            (23, 100, R2, 0), (16, 86, R1, 0), (21, 90, R3, 0),
                            (19, 96, SAND, 0), (22, 82, R2, 0), (17, 94, R1, -8)):
        a(spine(bx, sy - h, w, h, col, tilt))
        bx += w + 5

# ---- the lamp ---------------------------------------------------------------
a(f'<line x1="600" y1="0" x2="600" y2="84" stroke="{R2}" stroke-width="3"/>')
a(f'<path d="M 542 152 L 570 88 L 630 88 L 658 152 Z" fill="{R1}"/>')
a(f'<path d="M 556 120 L 578 96 L 596 96 L 572 120 Z" fill="{WHITE}" opacity="0.30"/>')
a(f'<rect x="538" y="150" width="124" height="9" rx="4.5" fill="{R2}"/>')
a(f'<path d="M 556 160 L 644 160 L 706 336 L 494 336 Z" fill="{WHITE}" opacity="0.13"/>')

# ---- the desk ---------------------------------------------------------------
a(f'<rect x="0" y="{FLOOR}" width="{W}" height="{H - FLOOR}" fill="{SAND}"/>')
a(f'<rect x="0" y="{FLOOR}" width="{W}" height="4" fill="{R2}" opacity="0.30"/>')
for cx_, rx_ in ((300, 100), (600, 136), (880, 108), (1030, 62)):
    a(f'<ellipse cx="{cx_}" cy="{FLOOR + 3}" rx="{rx_}" ry="10" fill="{R2}" opacity="0.10"/>')

# ---- left: the sheep, reading on a stack ------------------------------------
sy = FLOOR
for w, col, h in ((152, R2, 17), (140, SAND, 14), (158, R1, 16), (134, R3, 13)):
    sy -= h
    a(flat(196 + (158 - w) // 2, sy, w, col, h))
a(f'<image xlink:href="data:image/png;base64,{sheep}" x="208" y="{sy - 116}" '
  f'width="124" height="124"/>')

# ---- centre: the dinosaur and its open book ---------------------------------
a(f'<image xlink:href="data:image/png;base64,{dino}" x="514" y="298" width="186" height="186"/>')

BX, BY = 608, 472
a(f'<g transform="rotate(-2 {BX} {BY})">'
  f'<path d="M {BX - 140} {BY} q 70 -28 140 -6 q 70 -22 140 6 l 0 12 '
  f'q -70 -24 -140 -2 q -70 -22 -140 2 z" fill="{R1}"/>'
  f'<path d="M {BX - 132} {BY - 4} q 66 -26 132 -6 q 66 -20 132 6 l 0 -9 '
  f'q -66 -26 -132 -6 q -66 -20 -132 6 z" fill="{WHITE}"/>')
for i in range(5):
    yy = BY - 26 + i * 5
    a(f'<line x1="{BX - 108}" y1="{yy + 10}" x2="{BX - 24}" y2="{yy + 7}" stroke="{R3}" '
      f'stroke-width="1.8" opacity="0.85"/>')
    a(f'<line x1="{BX + 24}" y1="{yy + 7}" x2="{BX + 108}" y2="{yy + 10}" stroke="{R3}" '
      f'stroke-width="1.8" opacity="0.85"/>')
a(f'<line x1="{BX}" y1="{BY - 26}" x2="{BX}" y2="{BY + 6}" stroke="{R1}" stroke-width="2.5"/>'
  f'</g>')

a(flat(748, FLOOR - 15, 116, R2, 15))
a(flat(754, FLOOR - 28, 104, SAND, 13))

# ---- the thought ------------------------------------------------------------
a(f'<circle cx="716" cy="298" r="7" fill="{WHITE}"/>')
a(f'<circle cx="738" cy="272" r="11" fill="{WHITE}"/>')
a(f'<circle cx="800" cy="208" r="54" fill="{WHITE}"/>')
a(f'<path d="M 800 180 a 22 22 0 0 1 12 41 l 0 9 a 12 12 0 0 1 -24 0 l 0 -9 '
  f'a 22 22 0 0 1 12 -41 z" fill="{R1}"/>')
a(f'<rect x="792" y="232" width="17" height="5" rx="2.5" fill="{R2}"/>')
a(f'<rect x="794" y="240" width="13" height="4" rx="2" fill="{R2}"/>')
for ang in (-58, -90, -122):
    rad = math.radians(ang)
    a(f'<line x1="{800 + 62 * math.cos(rad):.1f}" y1="{208 + 62 * math.sin(rad):.1f}" '
      f'x2="{800 + 75 * math.cos(rad):.1f}" y2="{208 + 75 * math.sin(rad):.1f}" '
      f'stroke="{R1}" stroke-width="3.4" stroke-linecap="round"/>')

# ---- right: the sloth, already building -------------------------------------
a(f'<image xlink:href="data:image/png;base64,{sloth}" x="814" y="{FLOOR - 156}" '
  f'width="162" height="162"/>')
a(flat(990, FLOOR - 15, 104, R1, 15))
a(flat(996, FLOOR - 28, 92, SAND, 13))
a(flat(1002, FLOOR - 39, 80, R2, 11))

# ---- the only words ---------------------------------------------------------
a(f'<image xlink:href="data:image/png;base64,{logo}" x="62" y="{H - 116}" '
  f'width="74" height="72"/>')
a(f'<text x="150" y="{H - 66}" font-size="30" font-weight="700" letter-spacing="-1.1">'
  f'<tspan fill="{R1}">#</tspan><tspan fill="{INK}">KNOW-HOW</tspan></text>')
a(f'<text x="152" y="{H - 44}" font-size="11.5" font-weight="700" fill="{MUTED}" '
  f'letter-spacing="2.8">abap2UI5 SERIES</text>')
a(f'<text x="{W - 62}" y="{H - 46}" font-size="14" font-weight="700" fill="{R1}" '
  f'text-anchor="end">abap2UI5.org</text>')

a('</svg>')
svg = '\n'.join(o)

if not os.path.exists(CHROME):
    sys.exit(f'chromium not found at {CHROME} - adjust CHROME and re-run')

with tempfile.TemporaryDirectory() as tmp:
    page = os.path.join(tmp, 'page.html')
    with open(page, 'w') as f:
        f.write('<!doctype html><meta charset=utf-8>'
                f'<style>html,body{{margin:0;padding:0;overflow:hidden;background:{CREAM}}}'
                'svg{display:block}</style>')
        f.write(svg)
    raw = os.path.join(tmp, 'raw.png')
    subprocess.run([CHROME, '--headless', '--disable-gpu', '--no-sandbox', '--hide-scrollbars',
                    '--force-device-scale-factor=2', f'--window-size={W},{H + 230}',
                    f'--screenshot={raw}', 'file://' + page], check=True, capture_output=True)
    from PIL import Image
    Image.open(raw).convert('RGB').crop((0, 0, W * 2, H * 2)).save(OUT, optimize=True)

# the shelf must not grow past its plank
for sx, sy in SHELF:
    assert sx + 12 + sum(w + 5 for w, *_ in ((21,), (16,), (24,), (15,), (20,), (18,),
                                             (23,), (16,), (21,), (19,), (22,), (17,))) < sx + 356, \
        'the books outgrew the shelf'
print(f'wrote {OUT}  ({os.path.getsize(OUT) // 1024} KB)')
