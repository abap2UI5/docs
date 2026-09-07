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
 * ...AND WHICH OTHER DEPLOYMENTS THIS ORIGIN ACTUALLY SERVES.
 *
 * The gate above decides whether a cross-site link WORKS. It said nothing
 * about where it goes, and for months it went nowhere: /samples/,
 * /samples-controls/ and /samples-stack/ were three separate sites, each with
 * its own search page, and they were replaced by the one catalogue under
 * /playground/samples/. The manual went on naming all three - thirteen links
 * across seven pages, in What's Next, in Tooling, in the walkthrough, in the
 * VS Code and MCP chapters - because nothing looks at a link that leaves the
 * site and no build fails on one. An internal link that dies fails the build;
 * a link to the site next door just quietly stops being true.
 *
 * So: a cross-site link may name one of these paths and nothing else. Adding a
 * deployment here is one line; the point is that adding it is a decision
 * somebody makes rather than a URL nobody rereads.
 */
export const NEIGHBOURS = [
  '/playground/',           // the playground, and the sample catalogue under it
  '/linter/',               // the linter's rule reference
  '/web-abap2UI5-build/',   // a live build of the frontend, credited on Sponsor
];

/** What a retired deployment was replaced BY, so the error can say it. */
export const RETIRED = new Map([
  ['/samples/', 'the sample catalogue: /playground/samples/?src=learn'],
  ['/samples-controls/', 'the sample catalogue: /playground/samples/?src=controls'],
  ['/samples-stack/', 'the sample catalogue: /playground/samples/?src=stack'],
]);

/**
 * Is this a deployment that is still there? Takes the pathname of a URL that
 * `leavesTheSite` already accepted, and answers with the reason it is not.
 */
export function unreachable(pathname) {
  for (const at of NEIGHBOURS) if (pathname.startsWith(at)) return null;
  for (const [at, instead] of RETIRED) {
    if (pathname === at || pathname.startsWith(at)) return `retired — use ${instead}`;
  }
  return 'not a deployment this origin serves';
}

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
