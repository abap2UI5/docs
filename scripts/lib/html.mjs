/* The one thing done to every page on its way out of the build.
 *
 * The build and the theme are written in the house style, with the reasoning
 * for every decision beside it - and in a page that reasoning is an HTML
 * comment, which travels to the reader as bytes: 418 kB across the site, a
 * tenth of every page's compressed weight, and nobody reads a comment in a
 * served page. So they are stripped here, on the finished page, and the
 * source keeps every word.
 *
 * Outside <script> only. A script's text is what its hash in the policy is
 * taken from (scripts/lib/csp.mjs), so it must leave here byte for byte - and
 * a "<!--" inside JavaScript is legal JavaScript that is not this build's to
 * rewrite. The escaped "&lt;!--" in a listing is text, and never matched. */
export function stripComments(html) {
  return html
    .split(/(<script\b[\s\S]*?<\/script>)/)
    .map((part, i) => (i % 2 ? part : part.replace(/<!--[\s\S]*?-->/g, '')))
    .join('');
}
