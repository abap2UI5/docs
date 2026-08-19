/*
 * The Support page, as an icon at the right-hand end of the nav bar.
 *
 * Somebody who is stuck looks at the top of the page, not two scrolls down a
 * sidebar. It sits after the LinkedIn and GitHub icons, in the slot the default
 * theme leaves free there (`nav-bar-content-after`), and it is drawn to match
 * them — this is one more thing in that row, not a button competing with it.
 *
 * Why it is not a `socialLinks` entry, which is what that row is:
 * `VPSocialLink` hardcodes `target="_blank" rel="noopener"` and passes its
 * `link` through verbatim. Both are right for a link that leaves the site and
 * wrong for one that does not — a page of this documentation would open in a
 * second tab, and the href would miss the `/docs/` base and 404 on the
 * published site while working perfectly in a local dev server.
 */
import { h } from 'vue';
import { withBase } from 'vitepress';

/* A speech bubble: the Support page is "ask somebody" — an issue, or the Slack
 * channel — rather than a manual. Inline rather than one of Font Awesome's,
 * because that stylesheet comes from a CDN and an icon that is an empty square
 * until it arrives is worse than one that never needed it. */
const SPEECH_BUBBLE = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" aria-hidden="true">
  <path fill="currentColor" d="M12 3c5.52 0 10 3.36 10 7.5S17.52 18 12 18c-.86 0-1.7-.08-2.5-.24L4.5 20.5a.6.6 0 0 1-.88-.66l.86-3.3C2.9 15.2 2 13.44 2 11.5 2 6.36 6.48 3 12 3Z"/>
</svg>`;

/* Built rather than written as a path: the site is published under /docs/, and
 * `.html` because this build does not use clean URLs — the pages next to this
 * one are linked as `/docs/resources/support.html` too. Both are read from the
 * config rather than assumed; a raw `/resources/support` here is a 404 that
 * only shows up after a deploy. */
const SUPPORT = () => withBase('/resources/support.html');

export const SupportLink = {
  name: 'SupportLink',
  render: () =>
    h('a', {
      class: 'a2ui5-support',
      href: SUPPORT(),
      'aria-label': 'Support',
      title: 'Support',
      innerHTML: SPEECH_BUBBLE,
    }),
};
