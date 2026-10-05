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
  /* One scan, left to right, whichever comes first. Splitting at the scripts
     first read a comment that MENTIONS `<script>` as a script's start: the
     comment's tail survived, and so did everything up to the next
     `</script>` - which csp.mjs then reported as an inline script nobody
     wrote. A comment inside a script cannot start a match, because the script
     is consumed whole before the scan gets there. */
  return html.replace(/(<script\b[^>]*>[\s\S]*?<\/script>)|<!--[\s\S]*?-->/g, (m, script) => script ?? '');
}
