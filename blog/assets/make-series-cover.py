#!/usr/bin/env python3
"""The reusable cover for the whole abap2UI5 Know-How series.

Run from the repository root:  python3 blog/assets/make-series-cover.py
Writes blog/assets/series-cover.png at 2400x1260 (a 1200x630 design at 2x).

Unlike the per-article covers this one carries NOTHING article-specific: no
RTTS, no field catalog, no numbers. What it shows is the one mechanism every
article in the series stands on - an ABAP class implementing one interface, a
UI5 freestyle view, and the roundtrip between them. That stays true whatever
the article is about, which is the whole point of a series cover.

There is deliberately no episode number. The post carries that; a number baked
into the image is a second thing to keep in step.

Brand assets come from docs/public/ and are only scaled - docs/resources/logo.md
is explicit that brand art is never redrawn.
"""
import base64, os, re, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PUB = os.path.join(ROOT, 'docs', 'public')
OUT = os.path.join(ROOT, 'blog', 'assets', 'series-cover.png')
CHROME = '/opt/pw-browsers/chromium-1194/chrome-linux/chrome'

b64 = lambda p: base64.b64encode(open(os.path.join(PUB, p), 'rb').read()).decode()
logo, dino, sheep = b64('logo.png'), b64('mascots/dinosaur_brand.png'), b64('mascots/sheep_brand.png')

RED, DARK = '#D03C4A', '#A83232'
INK, MUTED = '#241F22', '#7C7278'
W, H = 1200, 630

CODE = """CLASS zcl_my_app DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
ENDCLASS.

METHOD z2ui5_if_app~main.

  me->client = client.
  IF client->check_on_event( ).
    on_event( ).
  ENDIF.

  client->view_display( view ).

ENDMETHOD."""

KW = r'\\b(CLASS|DEFINITION|PUBLIC|SECTION|INTERFACES|ENDCLASS|METHOD|ENDMETHOD|IF|ENDIF)\\b'
C_KW, C_CLS, C_TXT, C_FN = '#C792EA', '#7FDBCA', '#D6DEEB', '#82AAFF'

esc = lambda t: t.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')


def highlight(line):
    out, i = [], 0
    for m in re.finditer(KW + r'|\\b(z2ui5_if_app|zcl_my_app|client)\\b'
                         r'|\\b(check_on_event|on_event|view_display|main)\\b', line):
        out.append(f'<tspan fill="{C_TXT}">{esc(line[i:m.start()])}</tspan>')
        tok = m.group(0)
        col = (C_KW if re.fullmatch(KW, tok)
               else C_CLS if tok in ('z2ui5_if_app', 'zcl_my_app', 'client') else C_FN)
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
  <stop offset="0" stop-color="#FAFAFB"/><stop offset="1" stop-color="#F4F4F6"/>
</linearGradient>
<radialGradient id="glow" cx="0.10" cy="0.10" r="0.5">
  <stop offset="0" stop-color="{RED}" stop-opacity="0.05"/>
  <stop offset="1" stop-color="{RED}" stop-opacity="0"/>
</radialGradient>
<pattern id="dots" width="26" height="26" patternUnits="userSpaceOnUse">
  <path d="M 26 0 L 0 0 0 26" fill="none" stroke="#2A2A3A" stroke-width="0.6" opacity="0.045"/>
</pattern>
<filter id="panel" x="-15%" y="-15%" width="130%" height="140%">
  <feDropShadow dx="0" dy="8" stdDeviation="14" flood-color="#1A1A28" flood-opacity="0.13"/>
</filter>
<marker id="ar" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">
  <path d="M0,0 L10,5 L0,10 z" fill="{RED}"/>
</marker>
<marker id="ag" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto">
  <path d="M0,0 L10,5 L0,10 z" fill="#9C9599"/>
</marker>
</defs>''')

a(f'<rect width="{W}" height="{H}" fill="url(#bg)"/>')
a(f'<rect width="{W}" height="{H}" fill="url(#dots)"/>')
a(f'<rect width="{W}" height="{H}" fill="url(#glow)"/>')

# ---- left: the series mark ---------------------------------------------------
a(f'<image xlink:href="data:image/png;base64,{logo}" x="64" y="60" width="132" height="129"/>')
a(f'<text x="66" y="246" font-size="11.5" font-weight="700" fill="{MUTED}" '
  f'letter-spacing="3.8">abap2UI5 SERIES</text>')
a(f'<text x="64" y="304" font-size="50" font-weight="700" letter-spacing="-1.8">'
  f'<tspan fill="{RED}">#</tspan><tspan fill="{INK}">KNOW-HOW</tspan></text>')
a(f'<rect x="66" y="322" width="306" height="3" fill="{RED}"/>')
a(f'<text x="64" y="366" font-size="15.5" fill="{MUTED}">One ABAP class, one interface,</text>')
a(f'<text x="64" y="389" font-size="15.5" fill="{MUTED}">a UI5 view — and the roundtrip</text>')
a(f'<text x="64" y="412" font-size="15.5" fill="{MUTED}">between them.</text>')

# ---- right: the mechanism every article stands on ---------------------------
AX, AY, AW, AH = 440, 96, 302, 288
a(f'<g filter="url(#panel)"><rect x="{AX}" y="{AY}" width="{AW}" height="{AH}" rx="11" '
  f'fill="#0F1724"/></g>')
a(f'<rect x="{AX}" y="{AY}" width="{AW}" height="34" rx="11" fill="#182233"/>')
a(f'<rect x="{AX}" y="{AY + 23}" width="{AW}" height="11" fill="#182233"/>')
for i, c in enumerate(('#FF5F57', '#FEBC2E', '#28C840')):
    a(f'<circle cx="{AX + 19 + i * 17}" cy="{AY + 17}" r="5.2" fill="{c}"/>')
a(f'<text x="{AX + 82}" y="{AY + 22}" font-size="11" fill="#8FA3BF" '
  f'font-family="Menlo,Consolas,monospace">ABAP · backend</text>')
ly = AY + 56
for line in CODE.split('\n'):
    a(f'<text x="{AX + 18}" y="{ly}" font-size="10.6" xml:space="preserve" '
      f'font-family="Menlo,Consolas,monospace">{highlight(line)}</text>')
    ly += 15.4

BX, BY, BW, BH = 886, 96, 250, 288
a(f'<g filter="url(#panel)"><rect x="{BX}" y="{BY}" width="{BW}" height="{BH}" rx="11" '
  f'fill="#FFFFFF"/></g>')
a(f'<rect x="{BX}" y="{BY}" width="{BW}" height="34" rx="11" fill="#F2F3F5"/>')
a(f'<rect x="{BX}" y="{BY + 23}" width="{BW}" height="11" fill="#F2F3F5"/>')
a(f'<text x="{BX + 16}" y="{BY + 22}" font-size="11" fill="{MUTED}" '
  f'font-family="Menlo,Consolas,monospace">UI5 · sap.m freestyle</text>')
a(f'<rect x="{BX}" y="{BY + 34}" width="{BW}" height="38" fill="#354A5F"/>')
a(f'<text x="{BX + 16}" y="{BY + 59}" font-size="13.5" font-weight="700" fill="#FFFFFF">'
  f'My App</text>')
for r in range(4):
    ry = BY + 90 + r * 34
    a(f'<rect x="{BX + 16}" y="{ry}" width="{86 + (r % 3) * 22}" height="9" rx="4.5" fill="#DDE0E4"/>')
    a(f'<rect x="{BX + BW - 16 - 62}" y="{ry}" width="62" height="9" rx="4.5" fill="#EDEFF1"/>')
    a(f'<line x1="{BX + 16}" y1="{ry + 22}" x2="{BX + BW - 16}" y2="{ry + 22}" stroke="#F0F1F3"/>')
a(f'<rect x="{BX + 16}" y="{BY + 232}" width="88" height="28" rx="5" fill="{RED}"/>')
a(f'<text x="{BX + 60}" y="{BY + 251}" font-size="12" font-weight="700" fill="#FFFFFF" '
  f'text-anchor="middle">Save</text>')

# the roundtrip - two arrows, each labelled with what actually travels
MID = (AX + AW + BX) / 2
a(f'<path d="M {AX + AW + 8} 186 L {BX - 12} 186" stroke="{RED}" stroke-width="2.4" '
  f'fill="none" marker-end="url(#ar)"/>')
a(f'<text x="{MID}" y="176" font-size="10.5" font-weight="700" fill="{DARK}" '
  f'text-anchor="middle" font-family="Menlo,Consolas,monospace">view + model</text>')
a(f'<path d="M {BX - 8} 262 L {AX + AW + 12} 262" stroke="#9C9599" stroke-width="2.4" '
  f'fill="none" marker-end="url(#ag)"/>')
a(f'<text x="{MID}" y="252" font-size="10.5" font-weight="700" fill="{MUTED}" '
  f'text-anchor="middle" font-family="Menlo,Consolas,monospace">event + model</text>')
a(f'<rect x="{MID - 62}" y="213" width="124" height="22" rx="11" fill="#FFFFFF" '
  f'stroke="#DCDCE2"/>')
a(f'<text x="{MID}" y="228" font-size="9.5" fill="{MUTED}" text-anchor="middle" '
  f'font-family="Menlo,Consolas,monospace">ICF · one POST</text>')

# ---- mascots and footer -----------------------------------------------------
# a spec strip reads to an expert the way a slogan does not
SY = 486
a(f'<line x1="64" y1="{SY}" x2="{W - 64}" y2="{SY}" stroke="#DEDEE4"/>')
SPEC = [('interface', 'z2ui5_if_app'), ('frontend', 'sap.m · JSONModel'),
        ('transport', 'HTTP · JSON'), ('deploy', 'ICF node'),
        ('releases', '7.02 → ABAP Cloud'), ('licence', 'MIT')]
x = 64
for label, val in SPEC:
    a(f'<text x="{x}" y="{SY + 24}" font-size="9.5" font-weight="700" fill="#A6A2AA" '
      f'letter-spacing="1.6">{label.upper()}</text>')
    a(f'<text x="{x}" y="{SY + 45}" font-size="13" fill="{INK}" '
      f'font-family="Menlo,Consolas,monospace">{val}</text>')
    x += max(len(val) * 7.9, len(label) * 8.4) + 34

a(f'<image xlink:href="data:image/png;base64,{sheep}" x="{W - 168}" y="{H - 92}" '
  f'width="62" height="62" opacity="0.9"/>')
a(f'<image xlink:href="data:image/png;base64,{dino}" x="{W - 118}" y="{H - 98}" '
  f'width="68" height="68" opacity="0.9"/>')
a(f'<text x="64" y="{H - 30}" font-size="13" fill="{MUTED}">'
  f'Open source · <tspan font-weight="700" fill="{RED}">abap2UI5.org</tspan></text>')

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
    from PIL import Image
    Image.open(raw).convert('RGB').crop((0, 0, W * 2, H * 2)).save(OUT, optimize=True)

# every box must contain what it is drawn to contain, and the arrow labels
# must fit the gap - at the first attempt they sat 8px off the UI panel
code_last = AY + 56 + (len(CODE.split('\n')) - 1) * 15.4
assert code_last < AY + AH - 8, f'code overruns its panel: {code_last} vs {AY + AH}'
assert BY + BH < H - 40, 'ui panel runs into the footer'
longest = max(len(l) for l in CODE.split('\n')) * 6.4 + 36
assert longest < AW, f'code is wider than its panel: {longest:.0f} vs {AW}'
gap = BX - (AX + AW)
assert gap > len('event + model') * 6.3 + 40, f'arrow labels do not fit the gap: {gap}'
print(f'wrote {OUT}  ({os.path.getsize(OUT) // 1024} KB)')
