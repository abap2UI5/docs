#!/usr/bin/env python3
"""Cover image for blog article #1.

Run from the repository root:  python3 blog/assets/make-header.py
Writes blog/assets/01-rtti-header.png at 2400x1260 (a 1200x630 design at 2x).

The cover does NOT repeat the article's headline - a reader who is looking at
the cover is already on the article. It carries the line the article ends on
instead, which is the one worth remembering.

Brand assets (the mark, the mascots) come from docs/public/ and are only
scaled; docs/resources/logo.md is explicit that brand art is never redrawn.
Everything else is drawn here.

Rendering goes through the Chromium that ships with this environment, laying
out a page TALLER than the design and cropping the result, because --screenshot
clips a page to less than its --window-size height and would otherwise cut the
footer off.
"""
import base64, os, re, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PUB = os.path.join(ROOT, 'docs', 'public')
OUT = os.path.join(ROOT, 'blog', 'assets', '01-rtti-header.png')
CHROME = '/opt/pw-browsers/chromium-1194/chrome-linux/chrome'

b64 = lambda p: base64.b64encode(open(os.path.join(PUB, p), 'rb').read()).decode()
logo = b64('logo.png')
dino = b64('mascots/dinosaur_brand.png')
sheep = b64('mascots/sheep_brand.png')

RED, DARK = '#D03C4A', '#A83232'
INK, MUTED = '#241F22', '#7C7278'
W, H = 1200, 630

# ---- the code panel ---------------------------------------------------------
CODE = """METHOD render_any.

  DATA(comps) = CAST cl_abap_structdescr(
      CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data( tab )
      )->get_table_line_type( ) )->get_components( ).

  DATA(cols) = parent->ele( `Table`
      )->a( n = `items` v = client->_bind( tab )
      )->ele( `columns` ).

  " one column per component - discovered, not declared
  LOOP AT comps INTO DATA(comp).
    cols->ele( `Column`
        )->ele( `header`
            )->tag( `Text` )->a( n = `text` v = comp-name ).
  ENDLOOP.

ENDMETHOD."""

KW = r'\b(METHOD|ENDMETHOD|DATA|CAST|LOOP|AT|INTO|ENDLOOP)\b'
CLS = r'\b(cl_abap_structdescr|cl_abap_tabledescr|cl_abap_typedescr|client|parent)\b'
C_KW, C_STR, C_CLS, C_CMT, C_TXT, C_FN = '#C792EA', '#ECC48D', '#7FDBCA', '#5F7E97', '#D6DEEB', '#82AAFF'


def esc(t):
    return t.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')


def highlight(line):
    """hand-rolled ABAP tokenizer - comment, backtick string, keyword, class, call"""
    if line.lstrip().startswith('"'):
        return f'<tspan fill="{C_CMT}" font-style="italic">{esc(line)}</tspan>'
    out, i = [], 0
    for m in re.finditer(r'`[^`]*`|' + KW + '|' + CLS + r'|\b(ele|tag|a|describe_by_data|'
                         r'get_table_line_type|get_components|_bind)\b', line):
        out.append(f'<tspan fill="{C_TXT}">{esc(line[i:m.start()])}</tspan>')
        tok = m.group(0)
        col = (C_STR if tok.startswith('`') else
               C_KW if re.fullmatch(KW, tok) else
               C_CLS if re.fullmatch(CLS, tok) else C_FN)
        out.append(f'<tspan fill="{col}">{esc(tok)}</tspan>')
        i = m.end()
    out.append(f'<tspan fill="{C_TXT}">{esc(line[i:])}</tspan>')
    return ''.join(out)


o = []
a = o.append
a(f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
  f'width="{W}" height="{H}" viewBox="0 0 {W} {H}" '
  f'font-family="Helvetica Neue,Helvetica,Arial,sans-serif">')

a(f'''<defs>
<linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">
  <stop offset="0" stop-color="#FCE9EB"/><stop offset="0.55" stop-color="#FDF3F4"/>
  <stop offset="1" stop-color="#FBFAFA"/>
</linearGradient>
<radialGradient id="glow" cx="0.12" cy="0.18" r="0.55">
  <stop offset="0" stop-color="{RED}" stop-opacity="0.13"/>
  <stop offset="1" stop-color="{RED}" stop-opacity="0"/>
</radialGradient>
<pattern id="dots" width="16" height="16" patternUnits="userSpaceOnUse">
  <circle cx="1.5" cy="1.5" r="1.1" fill="{RED}" opacity="0.10"/>
</pattern>
<filter id="panel" x="-15%" y="-15%" width="130%" height="135%">
  <feDropShadow dx="0" dy="14" stdDeviation="20" flood-color="#3A1D24" flood-opacity="0.22"/>
</filter>
<filter id="card" x="-20%" y="-20%" width="145%" height="150%">
  <feDropShadow dx="0" dy="8" stdDeviation="12" flood-color="#3A1D24" flood-opacity="0.16"/>
</filter>
</defs>''')

a(f'<rect width="{W}" height="{H}" fill="url(#bg)"/>')
# a fat diagonal swash behind everything, so the page has a direction
a(f'<path d="M 720 -80 L 1320 -80 L 1320 720 L 380 720 Z" fill="{RED}" opacity="0.07" '
  f'transform="rotate(-8 850 320)"/>')
a(f'<rect width="{W}" height="{H}" fill="url(#dots)"/>')
a(f'<rect width="{W}" height="{H}" fill="url(#glow)"/>')

# ---- left column ------------------------------------------------------------
a(f'<image xlink:href="data:image/png;base64,{logo}" x="62" y="44" width="92" height="90"/>')

HL = 'font-size="43" font-weight="700" letter-spacing="-1.4"'
a(f'<text x="64" y="208" {HL} fill="{INK}">RTTS never went away.</text>')
a(f'<text x="64" y="264" {HL} fill="{INK}">Only</text>')
# marker swash under "the screen" - a slab, not a line
a(f'<g transform="rotate(-1.8 340 250)">'
  f'<rect x="176" y="228" width="252" height="48" rx="7" fill="{RED}"/></g>')
a(f'<text x="188" y="264" {HL} fill="#FFFFFF">the screen</text>')
a(f'<text x="64" y="320" {HL} fill="{INK}">in front of it did.</text>')

a(f'<text x="64" y="360" font-size="17" fill="{MUTED}">'
  f'The columns are whatever the internal table</text>')
a(f'<text x="64" y="384" font-size="17" fill="{MUTED}">'
  f'turns out to have — the field catalog, in a UI5 view.</text>')


def chip(x, y, text, solid=False):
    w = 15 + 8.15 * len(text)
    fill, stroke, col = ((RED, RED, '#FFFFFF') if solid
                         else ('#FFFFFF', RED, DARK))
    op = '' if solid else ' fill-opacity="0.75"'
    return (f'<g><rect x="{x}" y="{y}" width="{w:.0f}" height="38" rx="19" fill="{fill}"{op} '
            f'stroke="{stroke}" stroke-width="1.7"/>'
            f'<text x="{x + w / 2:.0f}" y="{y + 25}" font-size="14.5" font-weight="700" '
            f'fill="{col}" text-anchor="middle">{text}</text></g>'), w


x = 64
for t, solid in (('No entity type', True), ('No CDS view', False)):
    g, w = chip(x, 418, t, solid)
    a(g); x += w + 12
x = 64
for t, solid in (('No service binding', False), ('Any internal table', True)):
    g, w = chip(x, 470, t, solid)
    a(g); x += w + 12

a(f'<text x="64" y="{H - 26}" font-size="14" fill="{MUTED}">'
  f'Open source · MIT · abap2UI5 Know-How #1 · '
  f'<tspan font-weight="700" fill="{RED}">abap2UI5.org</tspan></text>')

# ---- code panel -------------------------------------------------------------
# tilted, so it reads as stuck onto the page rather than laid out on it
PX, PY, PW, PH = 636, 44, 520, 400
a(f'<g transform="rotate(-1.9 {PX + PW / 2} {PY + PH / 2})">')
a(f'<g filter="url(#panel)"><rect x="{PX}" y="{PY}" width="{PW}" height="{PH}" rx="12" '
  f'fill="#0F1724"/></g>')
a(f'<rect x="{PX}" y="{PY}" width="{PW}" height="38" rx="12" fill="#182233"/>')
a(f'<rect x="{PX}" y="{PY + 26}" width="{PW}" height="12" fill="#182233"/>')
for i, c in enumerate(('#FF5F57', '#FEBC2E', '#28C840')):
    a(f'<circle cx="{PX + 22 + i * 19}" cy="{PY + 19}" r="6" fill="{c}"/>')
a(f'<text x="{PX + 92}" y="{PY + 24}" font-size="12" fill="#8FA3BF" '
  f'font-family="Menlo,Consolas,monospace">z2ui5_cl_smp_app_497.clas.abap</text>')

ly = PY + 62
for line in CODE.split('\n'):
    a(f'<text x="{PX + 22}" y="{ly}" font-size="11.6" xml:space="preserve" '
      f'font-family="Menlo,Consolas,monospace">{highlight(line)}</text>')
    ly += 17
a('</g>')

# ---- the UI5 preview, overlapping the panel --------------------------------
CX, CY, CW, CH = 866, 424, 300, 160
a(f'<g transform="rotate(2.6 {CX + CW / 2} {CY + CH / 2})">')
a(f'<g filter="url(#card)"><rect x="{CX}" y="{CY}" width="{CW}" height="{CH}" rx="10" '
  f'fill="#FFFFFF"/></g>')
a(f'<rect x="{CX}" y="{CY}" width="{CW}" height="40" rx="10" fill="#F4F5F7"/>')
a(f'<rect x="{CX}" y="{CY + 28}" width="{CW}" height="12" fill="#F4F5F7"/>')
a(f'<text x="{CX + 16}" y="{CY + 26}" font-size="13.5" font-weight="700" fill="{INK}">Flights</text>')
a(f'<rect x="{CX + CW - 68}" y="{CY + 11}" width="52" height="19" rx="9.5" fill="{RED}" opacity="0.13"/>')
a(f'<text x="{CX + CW - 42}" y="{CY + 25}" font-size="10.5" font-weight="700" fill="{DARK}" '
  f'text-anchor="middle">RTTS</text>')

COLS = ['CARRID', 'CONNID', 'FLDATE', 'PRICE']
ROWS = [['LH', '0400', '2026-08-25', '899.00'],
        ['LH', '0402', '2026-08-26', '915.00'],
        ['AA', '0017', '2026-08-27', '422.50']]
cw = (CW - 32) / len(COLS)
hy = CY + 60
a(f'<line x1="{CX + 16}" y1="{hy + 8}" x2="{CX + CW - 16}" y2="{hy + 8}" stroke="#E6E7EA"/>')
for i, c in enumerate(COLS):
    tx = CX + 16 + i * cw + (cw - 4 if i == 3 else 0)
    a(f'<text x="{tx:.0f}" y="{hy}" font-size="9.5" font-weight="700" fill="{MUTED}" '
      f'letter-spacing="0.5" text-anchor="{"end" if i == 3 else "start"}">{c}</text>')
for r, row in enumerate(ROWS):
    ry = hy + 8 + (r + 1) * 28
    if r:
        a(f'<line x1="{CX + 16}" y1="{ry - 20}" x2="{CX + CW - 16}" y2="{ry - 20}" stroke="#F2F3F5"/>')
    for i, v in enumerate(row):
        tx = CX + 16 + i * cw + (cw - 4 if i == 3 else 0)
        a(f'<text x="{tx:.0f}" y="{ry - 6}" font-size="11.5" fill="{INK}" '
          f'font-weight="{"700" if i == 0 else "400"}" '
          f'text-anchor="{"end" if i == 3 else "start"}">{v}</text>')

a('</g>')

# ---- a sticker, slapped on the panel corner --------------------------------
# top-RIGHT: on the left it covered the filename and the first line of code
a(f'<g transform="rotate(11 1078 74)">'
  f'<rect x="988" y="48" width="180" height="52" rx="26" fill="#FFFFFF" stroke="{RED}" '
  f'stroke-width="3"/>'
  f'<text x="1078" y="81" font-size="21" font-weight="700" fill="{RED}" '
  f'text-anchor="middle">no OData!</text></g>')

# ---- mascots ----------------------------------------------------------------
a(f'<image xlink:href="data:image/png;base64,{sheep}" x="404" y="456" width="118" height="118"/>')
a(f'<image xlink:href="data:image/png;base64,{dino}" x="508" y="442" width="134" height="134"/>')
a(f'<g transform="rotate(-5 700 432)">'
  f'<rect x="626" y="404" width="150" height="54" rx="16" fill="#FFFFFF" stroke="{INK}" '
  f'stroke-width="3"/>'
  f'<path d="M 656 456 l -4 22 l 26 -20 z" fill="#FFFFFF" stroke="{INK}" stroke-width="3" '
  f'stroke-linejoin="round"/>'
  f'<rect x="640" y="452" width="42" height="7" fill="#FFFFFF"/>'
  f'<text x="701" y="440" font-size="24" font-weight="700" fill="{INK}" '
  f'text-anchor="middle">still here!</text></g>')

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
    subprocess.run([CHROME, '--headless', '--disable-gpu', '--no-sandbox', '--hide-scrollbars',
                    '--force-device-scale-factor=2', f'--window-size={W},{H + 230}',
                    f'--screenshot={raw}', 'file://' + page], check=True, capture_output=True)
    try:
        from PIL import Image
    except ImportError:
        sys.exit('needs pillow for the crop:  pip install pillow')
    Image.open(raw).convert('RGB').crop((0, 0, W * 2, H * 2)).save(OUT, optimize=True)

print(f'wrote {OUT}  ({os.path.getsize(OUT) // 1024} KB)')
