---
outline: [2, 4]
---
# Logo & Brand

Everything the project signs — the documentation, the repositories, a talk
slide, a link shared on LinkedIn — uses the same red mark, and the same three
animals turn up around it. This page is the collection: every asset the project
puts its name on, the colours behind them, and what may be done with them.

The files are the originals. Download them from the links, and prefer them over
a screenshot of one, a copy pasted out of a slide deck, or a copy re-uploaded
somewhere else — those drift in colour and lose their transparency.

## The Mark

The abap2UI5 mark: the wordmark set on two lines, tilted, knocked out of a red
circle. This is the primary asset — where only one image fits, this is the one.

![The abap2UI5 mark: white wordmark on a red circle](/logo.png){width=200}

| | |
| --- | --- |
| File | [`logo.png`](/logo.png) |
| Size | 1064 × 1042, PNG, transparent background |
| Used by | the nav bar and the home page hero of this site |

The background is transparent, so the mark sits on whatever is behind it. On a
dark surface the white letters stay readable, because they are knocked out of
the red rather than printed on it.

## The Mark on White

The same mark with a white background baked in, for the places that cannot
handle transparency — an older presentation tool, a print shop, a chat client
that flattens a PNG onto black.

![The abap2UI5 mark on a white background](/logo_white_back.png){width=200}

| | |
| --- | --- |
| File | [`logo_white_back.png`](/logo_white_back.png) |
| Size | 1193 × 1081, PNG, opaque white background |

If the surface behind it is white anyway, use [the mark](#the-mark) — a
transparent file cannot go wrong on a white page, and an opaque one shows its
edges the day the page turns grey.

## Favicon

The mark at tab size, as an `.ico`. The browser tab of this documentation, and
the tab of any abap2UI5 app that does not
[set its own](/configuration/setup/favicon).

![The abap2UI5 favicon](/favicon.ico){width=64}

| | |
| --- | --- |
| File | <a href="/docs/favicon.ico"><code>favicon.ico</code></a> |
| Size | 256 × 251, ICO |
| Used by | `head` in `docs/.vitepress/config.mjs` |

## Link Preview

What LinkedIn, Slack, WhatsApp and X show when a documentation link is shared:
the mark, the name and the one-line claim, on white. 1200 × 630 — the ratio
those platforms render as a large card rather than a thumbnail.

![The abap2UI5 link preview card](/og-image.png){width=480}

| | |
| --- | --- |
| File | [`og-image.png`](/og-image.png) |
| Size | 1200 × 630, PNG |
| Used by | the `og:image` every page of this site carries |

Every page shares this one image. The title and description under it are
per-page — `transformPageData` in `docs/.vitepress/config.mjs` fills them in —
so a shared link names the page it points at, under the project's picture.

## Code Cover

The project in three lines of ABAP: a `main` method that displays a message
box. Used as a cover image where a mark alone says too little — an article
header, a slide, a repository social preview.

![Three lines of ABAP: a main method displaying a Hello World message box](/code_cover.png)

| | |
| --- | --- |
| File | [`code_cover.png`](/code_cover.png) |
| Size | 2278 × 830, PNG, transparent background |

## Mascots

Three animals travel with the project — on posts, slides and covers, where the
mark alone is a logo and an animal is a picture. They are drawings, not marks:
they stand next to the logo, never in place of it.

Each animal comes in three files, the same way the mark does: a transparent
PNG to use, a copy on white for the places that cannot handle transparency, and
the artwork as it was drawn.

Every animal arrived on a red of its own — `#C12E2B`, `#AB2319` and, under the
JPEG noise, roughly `#F5021B` — three reds that are each almost, but not quite,
the red of the mark. The brand copies are shifted onto `#D03C4A`, so a mascot
next to the logo is the same red as the logo. Only the red ink moved, in
proportion to how much of it a pixel carries: outlines, shading and the sloth's
cream fur are untouched.

The transparent copies are cut out along the drawing, not along its white. The
white behind the sheep and the white of its wool are the same white, so the
background was found by flooding in from the border — white that no path from
the edge reaches is inside the animal and stays: the wool, the monitor, the
keyboard. The soft grey edge these drawings carry is dropped rather than kept
half-transparent, which is what would otherwise show as a grey halo on every
background that is not white.

### Sloth

The sloth at the keyboard — unhurried, and the work still gets done.

![The abap2UI5 sloth, at a computer](/mascots/sloth_brand.png){width=200}

| | |
| --- | --- |
| Use | [`sloth_brand.png`](/mascots/sloth_brand.png) — 400 × 400, PNG, transparent |
| On white | [`sloth_brand_white_back.png`](/mascots/sloth_brand_white_back.png) |
| As drawn | [`sloth.jpg`](/mascots/sloth.jpg) — a JPEG, so it carries compression artefacts along the outline; the copies above are PNGs and add none of their own |
| Red | ≈ `#F5021B` → `#D03C4A` |

### Dinosaur

![The abap2UI5 dinosaur](/mascots/dinosaur_brand.png){width=200}

| | |
| --- | --- |
| Use | [`dinosaur_brand.png`](/mascots/dinosaur_brand.png) — 400 × 400, PNG, transparent |
| On white | [`dinosaur_brand_white_back.png`](/mascots/dinosaur_brand_white_back.png) |
| As drawn | [`dinosaur.png`](/mascots/dinosaur.png) |
| Red | `#C12E2B` → `#D03C4A` |

### Sheep

![The abap2UI5 sheep](/mascots/sheep_brand.png){width=200}

| | |
| --- | --- |
| Use | [`sheep_brand.png`](/mascots/sheep_brand.png) — 400 × 400, PNG, transparent |
| On white | [`sheep_brand_white_back.png`](/mascots/sheep_brand_white_back.png) |
| As drawn | [`sheep.png`](/mascots/sheep.png) |
| Red | `#AB2319` → `#D03C4A` |

What is white inside an animal stays white — the wool, the screen, the keys.
On a dark background that white is what you see, because it is part of the
drawing and not the surface behind it.

## Colours

Two reds and white. The mark, and every mascot above, uses the first red and
white only; the second red exists for interface states, where the first one has
to darken under a cursor.

| | Hex | Where |
| --- | --- | --- |
| <span style="display:inline-block;width:1.15em;height:1.15em;vertical-align:-0.2em;border-radius:3px;background:#D03C4A"></span> Red | `#D03C4A` | the circle in the mark, and `--vp-c-brand-1` — links, buttons, the hero name |
| <span style="display:inline-block;width:1.15em;height:1.15em;vertical-align:-0.2em;border-radius:3px;background:#A83232"></span> Dark red | `#A83232` | `--vp-c-brand-2` — the hover state of the above, nothing in the mark |
| <span style="display:inline-block;width:1.15em;height:1.15em;vertical-align:-0.2em;border:1px solid var(--vp-c-divider);border-radius:3px;background:#FFFFFF"></span> White | `#FFFFFF` | the wordmark knocked out of the circle |

The site's own tokens are set in `docs/.vitepress/theme/style.css`. Take the hex
values from this table rather than picking them out of a screenshot with a
colour dropper: a PNG scaled in a browser hands you an interpolated pixel,
which is a red that appears nowhere in the brand.

## Using the Mark

abap2UI5 is [MIT licensed](/resources/license), and that covers these files
along with the rest of the repository. The rules below are therefore a request,
not a licence term — but they are what keeps the mark recognisable:

- **Use it to refer to the project.** A talk, a blog post, a slide about your
  integration, a badge in your readme — no permission needed.
- **Do not redraw or recolour it.** The red is `#D03C4A`, the circle is a
  circle, the wordmark is not retyped in another font. Scale it; that is all.
- **Do not stretch it.** Both files are close to square but not exactly square.
  Set one dimension and let the other follow.
- **Leave it room.** Roughly half the circle's diameter of clear space on every
  side, free of other logos and text.
- **Do not imply endorsement.** The mark next to your product name reads as "the
  project ships this". Say "built with abap2UI5" instead — that is accurate and
  needs no mark at all.
- **The animals are not the mark.** The same rules cover them, with one more:
  a mascot may sit next to the logo, illustrate a slide or head a post, but the
  thing that identifies the project is the red circle.

For anything this page does not answer — a print resolution, a variant that
does not exist here, permission for something the list above rules out — write
to <contact@abap2UI5.org>.
