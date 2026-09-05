/*
 * Which links leave this deployment without leaving the origin — and why every
 * one of them has to carry a `target`.
 *
 * The documentation, the playground, the sample catalogue and the linter's
 * rule pages are separate deployments on ONE origin
 * (abap2ui5.github.io/docs, /playground, /playground/samples, /linter). That
 * shared origin is deliberate: it is what lets the four bars share a theme and
 * a position memory through one localStorage. It is also what breaks a plain
 * link between them.
 *
 * This site is a single page application. VitePress's router listens for
 * clicks on the whole window and takes any link over that is same-origin and
 * looks like a page — `origin === currentUrl.origin && treatAsHtml(pathname)`,
 * in vitepress/dist/client/app/router.js. `/playground/` passes both tests, so
 * the router pushed the URL, looked for a page of THIS site to render at it,
 * found none — the playground is another deployment, not a page of the manual
 * — and drew this site's own 404 in its place. The address bar said
 * /playground/ and the document said PAGE NOT FOUND. A reload loaded the real
 * page, which is exactly what a failed SPA route change looks like from the
 * outside, and is why this reads as "sometimes broken" rather than as a dead
 * link.
 *
 * Every way out of the manual was affected: both bar items, the Linter rules
 * row in the menu, and the Run bar's "Switch to Playground with this code".
 *
 * The router's own escape hatch is the line above those two tests — a link
 * carrying a `target` attribute is left alone, whatever the value — so
 * `target="_self"` opts a link out of the SPA and keeps the one-tab behaviour
 * the four bars promise. That is what this module decides: which links need
 * it, so scripts/check-cross-site.mjs can hold the built site to it.
 *
 * A link to another HOST needs nothing. The router never looks at github.com,
 * and VitePress's markdown renderer gives every external link in a page a
 * `target="_blank"` of its own. Only hand-written markup pointing at
 * abap2ui5.github.io outside /docs/ can get this wrong, which is why it did.
 */

/** Where this site is published, and the path that is its own. */
export const SITE = {
  origin: 'https://abap2ui5.github.io',
  base: '/docs/',
};

/*
 * VitePress's own extension list, copied out of
 * vitepress/dist/client/shared.js rather than imported: this runs in Node
 * against built HTML, that ships to a browser, and a gate that silently
 * followed a dependency's internal module would stop being a statement about
 * what the router does. Copied, like the palette the three sites share — with
 * the same rule: when it moves over there, move it here.
 *
 * It matters because it is the difference between a link that needs the
 * attribute and one that does not. /playground/ has no extension, so the
 * router treats it as a page and takes it over. /samples/catalogue.json ends
 * in a known extension, so the router leaves it alone and no attribute is
 * needed — demanding one there would be a rule nobody could justify from what
 * the router does.
 */
const KNOWN_EXTENSIONS = new Set(
  ('3g2,3gp,aac,ai,apng,au,avif,bin,bmp,cer,class,conf,crl,css,csv,dll,'
    + 'doc,eps,epub,exe,gif,gz,ics,ief,jar,jpe,jpeg,jpg,js,json,jsonld,m4a,'
    + 'man,mid,midi,mjs,mov,mp2,mp3,mp4,mpe,mpeg,mpg,mpp,oga,ogg,ogv,ogx,'
    + 'opus,otf,p10,p7c,p7m,p7s,pdf,png,ps,qt,roff,rtf,rtx,ser,svg,t,tif,'
    + 'tiff,tr,ts,tsv,ttf,txt,vtt,wav,weba,webm,webp,woff,woff2,xhtml,xml,'
    + 'yaml,yml,zip').split(','),
);

/** Would the router treat this path as a page of the site? */
export function treatAsHtml(pathname) {
  const ext = pathname.split('.').pop();
  return ext == null || !KNOWN_EXTENSIONS.has(ext.toLowerCase());
}

/**
 * The URL this href points at, if following it leaves this deployment while
 * staying on its origin — the one case the router gets wrong. `null` for
 * everything else: a page of this site, another host, a mailto:, a file the
 * router would not touch.
 *
 * `from` is the page the link is written on, and it is not optional in
 * practice: a cookbook page links its neighbours as `./../model/x.html`, and
 * resolving that against the site ROOT rather than against the page turns a
 * dozen ordinary in-site links into cross-site ones. The default is the front
 * page, for a caller holding a link and no page.
 */
export function leavesTheSite(href, from = SITE.origin + SITE.base) {
  let url;
  try {
    /* Resolved the way the browser resolves it on that page, so the absolute
     * form, the root-relative one and the relative one are one case. */
    url = new URL(href, new URL(from, SITE.origin));
  } catch {
    return null;
  }
  if (url.protocol !== 'http:' && url.protocol !== 'https:') return null;
  if (url.origin !== SITE.origin) return null;
  if (url.pathname.startsWith(SITE.base)) return null;
  if (!treatAsHtml(url.pathname)) return null;
  return url;
}

/* An <a> tag with its attributes, quotes respected so a `>` inside a title
 * does not end the tag early. */
const TAG = /<a\b((?:"[^"]*"|'[^']*'|[^>"'])*)>/gi;
const attribute = (attrs, name) => {
  const m = new RegExp(`(?:^|\\s)${name}\\s*=\\s*(?:"([^"]*)"|'([^']*)'|([^\\s>]+))`, 'i').exec(attrs);
  return m ? (m[1] ?? m[2] ?? m[3]) : null;
};

/**
 * Every link in one document that leaves this deployment for a neighbouring
 * one, each with the two things that decide whether it works: whether it
 * carries a `target`, and whether it is a download (the router's other
 * exemption). `from` is the page's own URL, which its relative links are
 * resolved against.
 *
 * What this cannot see is a link built in the browser — the Run bar's
 * "Switch to Playground with this code" is created by
 * docs/.vitepress/theme/playground.js from a URL the playground's loader
 * returns, and is nowhere in the HTML. It carries the attribute where it is
 * created, and test/cross-site.test.mjs pins that.
 */
export function crossSiteLinks(html, from) {
  const found = [];
  for (const [, attrs] of html.matchAll(TAG)) {
    const href = attribute(attrs, 'href');
    if (href == null) continue;
    const url = leavesTheSite(href, from);
    if (!url) continue;
    found.push({
      href,
      url: url.href,
      exempt: /(?:^|\s)target\s*=/i.test(attrs) || /(?:^|\s)download(?:[\s=]|$)/i.test(attrs),
    });
  }
  return found;
}
