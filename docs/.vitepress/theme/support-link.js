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

/* A speech bubble with a smiling face: the Support page is "ask somebody" — an
 * issue, or the Slack channel — rather than a manual, and the face says a
 * person answers on the other side. Drawn as an outline so it reads as one
 * shape at the 20px the nav bar gives it; the bubble stops short of the face
 * instead of running behind it, because a stroke crossing a stroke turns into
 * a smudge at that size.
 *
 * Inline rather than one of Font Awesome's, because that stylesheet comes from
 * a CDN and an icon that is an empty square until it arrives is worse than one
 * that never needed it. */
const SPEECH_BUBBLE = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
  <path d="M11.25 4.25H5A2.75 2.75 0 0 0 2.25 7v6.5A2.75 2.75 0 0 0 5 16.25h.93v2.63a.6.6 0 0 0 .97.47l4-3.1h7.6a2.75 2.75 0 0 0 2.75-2.75V12"/>
  <circle cx="17.5" cy="6.5" r="4.9"/>
  <circle cx="15.9" cy="5.3" r=".85" fill="currentColor" stroke="none"/>
  <circle cx="19.1" cy="5.3" r=".85" fill="currentColor" stroke="none"/>
  <path d="M15.6 8.2a2.45 2.45 0 0 0 3.8 0"/>
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
