/* The policy every page of the manual is published under, and the two
 * helpers that keep it honest: the hash of an inline script as CSP spells it,
 * and the inline scripts a finished page actually carries.
 *
 * Why a policy at all: three deployments share abap2ui5.github.io, and the
 * playground runs whatever ABAP a shared link carries - in a worker, then in
 * a frame, on this origin. A page of the manual runs nothing of the kind, so
 * it says so, and a script that reached one of these pages by any road but
 * the build - a compromised dependency, a stray <script> in a markdown file,
 * an injected handler - stops here rather than running on the shared origin.
 *
 * What is allowed, and why each thing is:
 *   script-src   this origin (site.js, search.mjs), the playground beside it
 *                (the Run panel's loader, which on the published site IS this
 *                origin and on a dev server is not), and the two inline
 *                scripts by hash - the theme line and the borrowed menu
 *                script. No 'unsafe-inline', which is the whole point.
 *   style-src    'unsafe-inline' has to stay: the highlighter puts a colour
 *                pair on every token as a style attribute, and <noscript>
 *                carries a <style>.
 *   img-src      this origin, data: URIs, and the two hosts the pages still
 *                take screenshots from: github.com, whose user-attachments
 *                URLs answer with a redirect to *.githubusercontent.com. A
 *                host missing here is a picture the page silently loses, so
 *                test/csp.test.mjs walks every page for the hosts its images
 *                name and holds them against this list.
 *   connect-src  this origin (the search index) and the playground.
 *   frame-src    this origin and the playground (the Run panel's frame).
 *   object-src   none. base-uri and form-action: this origin.
 *
 * A <meta> policy cannot carry frame-ancestors or report-uri, and this one
 * needs neither. */
import { createHash } from 'node:crypto';

/** The neighbouring deployments, which on the published site are 'self'. */
export const NEIGHBOUR = 'https://abap2ui5.github.io';

/** Where a page may still load a picture from besides this origin. */
export const IMAGE_HOSTS = ['https://github.com', 'https://*.githubusercontent.com'];

/** Whether `url` is a picture the policy lets a page load. */
export function imageAllowed(url, origin = NEIGHBOUR) {
  let u;
  try { u = new URL(url); } catch { return false; }
  if (u.origin === origin || u.protocol === 'data:') return true;
  return IMAGE_HOSTS.some((host) => host.startsWith('https://*.')
    ? u.protocol === 'https:' && u.hostname.endsWith(host.slice('https://*'.length))
    : u.origin === host);
}

/** An inline script's text, as CSP names it: sha256, base64, quoted. */
export const hashOf = (script) => `'sha256-${createHash('sha256').update(script, 'utf8').digest('base64')}'`;

/** The policy, with these inline scripts - and only these - allowed. */
export function contentSecurityPolicy(inlineScripts) {
  const hashes = inlineScripts.map(hashOf);
  return [
    "default-src 'self'",
    ['script-src', "'self'", NEIGHBOUR, ...hashes].join(' '),
    "style-src 'self' 'unsafe-inline'",
    ['img-src', "'self'", 'data:', ...IMAGE_HOSTS].join(' '),
    "font-src 'self'",
    `connect-src 'self' ${NEIGHBOUR}`,
    `frame-src 'self' ${NEIGHBOUR}`,
    "object-src 'none'",
    "base-uri 'self'",
    "form-action 'self'",
  ].join('; ');
}

/**
 * The inline scripts a page carries - the text of every <script> with no src
 * that the browser would run. A data block (application/ld+json) is not run,
 * so it is not one.
 */
export function inlineScriptsIn(html) {
  return [...html.matchAll(/<script(?![^>]*\bsrc=)([^>]*)>([\s\S]*?)<\/script>/g)]
    .filter((m) => !/\btype="application\/ld\+json"/.test(m[1]))
    .map((m) => m[2]);
}
