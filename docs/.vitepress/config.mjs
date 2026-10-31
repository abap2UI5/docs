import { defineConfig } from "vitepress";
import { playgroundButton } from "./playground.mjs";
import { tableScroll, codeTitle } from "./markup.mjs";

// Where the site is actually served from. Link previews (LinkedIn, Slack,
// WhatsApp, X) only accept ABSOLUTE urls in og:image / og:url — a relative
// "/docs/og-image.png" is silently dropped and the preview falls back to the
// grey placeholder card.
const SITE_URL = "https://abap2ui5.github.io/docs";
// 1200x630 — the ratio LinkedIn renders as a large card. Anything narrower
// than 1200px is shown as a small square thumbnail instead.
const OG_IMAGE = `${SITE_URL}/og-image.png`;

// https://vitepress.dev/reference/site-config
export default defineConfig({
  // A Run button under every fenced example the playground can actually
  // start. Which ones those are is decided in ./playground.mjs.
  markdown: {
    // Order matters: `codeTitle` wraps whatever the fence renderer before it
    // produced, so the Run button ends up INSIDE the titled block rather than
    // beside it.
    config: (md) => {
      tableScroll(md);
      playgroundButton(md);
      codeTitle(md);
    },
  },
  lastUpdated: {
    text: "Updated at",
    formatOptions: {
      dateStyle: "full",
      timeStyle: "medium",
    },
  },
  base: "/docs/", // Set your base URL here
  /* `docs/public/` is the raw markdown of this manual — one .md per page, for
   * an agent following llms.txt — and VitePress copies it to the root of the
   * site, which is where llms.txt points (/docs/cookbook/model/trees.md).
   *
   * It was ALSO being globbed as source, because srcDir is `docs/` and nothing
   * said otherwise. So every page of the manual was rendered a second time, as
   * a full themed page at /docs/public/cookbook/model/trees.html: 165 files
   * nothing links to, nothing indexes (generate-search.mjs walks the real
   * pages) and no sidebar names — which is how they were found, being the only
   * pages on the site whose crumb line could say nothing but "Documentation".
   * The raw markdown is unaffected: publicDir copies the files regardless of
   * whether they are also treated as pages. */
  srcExclude: ["public/**"],
  head: [
    ["link", { rel: "shortcut icon", href: "/docs/favicon.ico" }],
    [
      "link",
      { rel: "apple-touch-icon", sizes: "180x180", href: "/docs/apple-touch-icon.png" },
    ],
    // THE TYPE. One file, from this origin, and preloaded because it is what
    // the first line of every page is set in — a `@font-face` is only
    // discovered once the stylesheet has been parsed, which is a round trip
    // later than it needs to be. The italic is not preloaded: it is fetched by
    // the pages that have any, and most have none.
    //
    // Self-hosted, not requested from a font CDN. Montserrat and Fira Code
    // used to be asked for here and were applied by nothing — no rule in
    // theme/style.css, or anywhere else, ever named either one — so the site
    // paid for two stylesheets and four font files it never showed a glyph
    // from. What is below is the opposite case: one face, named by every rule
    // on the site, and the reader's own stack behind it if it never arrives.
    [
      "link",
      {
        rel: "preload",
        href: "/docs/fonts/inter-roman-latin.woff2",
        as: "font",
        type: "font/woff2",
        crossorigin: "",
      },
    ],
    //
    // Font Awesome went the same way, one release later. It was loaded on
    // EVERY page of the site — a render-blocking stylesheet plus the webfont
    // behind whichever glyph was used — to draw four icons: the three on the
    // home page's feature cards and one GitHub mark on configuration/
    // launchpad. All four are inline SVG now, in the markdown that shows
    // them, next to the marks in the home page's second row of cards that
    // were already drawn that way. The argument is the one written over
    // those: an icon that is an empty square until a stylesheet from a CDN
    // arrives is worse than one that never needed it — and on a blocked or
    // slow CDN the feature icons did not degrade to a square, they were
    // simply not there. Adding an icon means adding an `<svg>`, not a class.
    // Link preview card — the per-page og:title, og:description and og:url are
    // added in transformPageData below.
    ["meta", { property: "og:type", content: "website" }],
    ["meta", { property: "og:site_name", content: "abap2UI5" }],
    ["meta", { property: "og:image", content: OG_IMAGE }],
    ["meta", { property: "og:image:type", content: "image/png" }],
    ["meta", { property: "og:image:width", content: "1200" }],
    ["meta", { property: "og:image:height", content: "630" }],
    [
      "meta",
      {
        property: "og:image:alt",
        content: "abap2UI5 — Build UI5 Apps Purely in ABAP",
      },
    ],
    ["meta", { name: "twitter:card", content: "summary_large_image" }],
    ["meta", { name: "twitter:image", content: OG_IMAGE }],
  ],
  title: "abap2UI5",
  description: "Build UI5 Apps Purely in ABAP",
  // Every page gets its own og:title / og:description / og:url, so a shared
  // link shows the page it points at instead of the site front page.
  transformPageData(pageData) {
    const url = `${SITE_URL}/${pageData.relativePath}`
      .replace(/index\.md$/, "")
      .replace(/\.md$/, ".html");
    // `||`, not `??`: a page without frontmatter carries an EMPTY STRING here,
    // not undefined, and an empty og:description is what a preview card shows
    // as a blank line.
    const pageTitle = pageData.frontmatter.title || pageData.title;
    const title =
      pageData.relativePath === "index.md" || !pageTitle
        ? "abap2UI5 — Build UI5 Apps Purely in ABAP"
        : `${pageTitle} | abap2UI5`;
    const description =
      pageData.frontmatter.description ||
      pageData.description ||
      "Build UI5 Apps Purely in ABAP";

    pageData.frontmatter.head ??= [];
    pageData.frontmatter.head.push(
      ["link", { rel: "canonical", href: url }],
      ["meta", { property: "og:url", content: url }],
      ["meta", { property: "og:title", content: title }],
      ["meta", { property: "og:description", content: description }],
      ["meta", { name: "twitter:title", content: title }],
      ["meta", { name: "twitter:description", content: description }],
    );
  },
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    //
    // THE DEFAULT THEME'S BAR, AS EVERY VITEPRESS SITE HAS IT: the logo and
    // the name on the left, the search, the sections of the manual, the
    // version, the light/dark switch and the two marks. Nothing of our own:
    // a developer who has used one VitePress site knows where everything is.
    logo: "/logo.png",
    siteTitle: "abap2UI5",
    // The logo leads to the abap2UI5 home page, like the logo on every other
    // abap2UI5 site (CONVENTION.md in abap2UI5/abap2UI5.github.io). Another
    // deployment on the same origin, hence target _self (check:cross-site).
    logoLink: { link: "https://abap2ui5.github.io/", target: "_self" },
    footer: {
      message: `
      <a href="/docs/resources/license">License</a> |
      <a href="/docs/resources/support#contact">Contact</a>`,
      copyright: `Copyright © 2023-${new Date().getFullYear()} abap2UI5`,
    },
    editLink: {
      pattern:
        "https://github.com/abap2UI5/docs/tree/main/docs/:path",
      text: "Edit this page on GitHub",
    },
    // VitePress's own search, over the pages of this manual. The samples have
    // their own search on their own site.
    search: { provider: "local" },
    // The sections of the manual, each opening its part of the sidebar, and
    // the released version with what belongs to it. The version is what
    // check:version reads (scripts/lib/release.mjs).
    nav: [
      { text: "Guide", link: "/get_started/about", activeMatch: "^/(get_started|configuration|advanced)/" },
      { text: "Tutorial", link: "/tutorials/", activeMatch: "^/tutorials/" },
      { text: "Cookbook", link: "/cookbook/", activeMatch: "^/cookbook/" },
      { text: "API", link: "/resources/api", activeMatch: "^/resources/" },
      {
        text: "1.148.0",
        items: [
          { text: "Release notes", link: "/resources/changelog" },
          { text: "Support", link: "/resources/support" },
          { text: "Contribute", link: "/resources/contribution" },
          { text: "Sponsor", link: "/resources/sponsor" },
        ],
      },
    ],
    sidebar: [
      {
        // A SEQUENCE, AND IT READS AS ONE. What it is, install it, see one app
        // run, then the assistant - in the order a newcomer meets them. It
        // used to be five entries with a "Quickstart" group in the middle and
        // a "What's Next?" page at the end that repeated this sidebar; the
        // group is gone (its two pages stand in the row) and so is the page
        // (its links live at the end of Hello World, where a reader who has
        // just seen an app run is actually asking the question). The label
        // of every entry is the page's own H1, which
        // test/sidebar-titles.test.mjs holds for the whole menu.
        text: "Getting Started",
        link: "/get_started/about",
        collapsed: true,
        items: [
          { text: "In a Nutshell", link: "/get_started/about" }, // sidebar
          { text: "Quickstart", link: "/get_started/quickstart" },
          { text: "Hello World", link: "/get_started/hello_world" },
          { text: "Developing with AI", link: "/get_started/ai" },
        ],
      },
      {
        // Between Getting Started and the Cookbook on purpose: a tutorial is
        // read after the framework is installed and before the reference
        // chapters, and that is the order the three sections stand in.
        text: "Tutorial",
        link: "/tutorials/",
        collapsed: true,
        // The section's own row opens the tutorial's index page, and the
        // steps stand directly under it. They used to sit one level deeper,
        // in a "Walkthrough" group that was the only thing in the section -
        // two rows in front of the reader for one tutorial, and a caret that
        // opened onto a second label before the first step.
        items: [
          { text: "1. The App Class", link: "/tutorials/step-1" },
          { text: "2. A First View", link: "/tutorials/step-2" },
          { text: "3. Events", link: "/tutorials/step-3" },
          { text: "4. Data Binding", link: "/tutorials/step-4" },
          { text: "5. List Binding", link: "/tutorials/step-5" },
          { text: "6. Row Events", link: "/tutorials/step-6" },
          { text: "7. Popups", link: "/tutorials/step-7" },
          { text: "8. Selection Screen", link: "/tutorials/step-8" },
          { text: "9. Tables", link: "/tutorials/step-9" },
          { text: "10. App Structure", link: "/tutorials/step-10" },
          { text: "11. To Production", link: "/tutorials/step-11" },
          { text: "12. Unit Tests", link: "/tutorials/step-12" },
        ],
      },
      {
        // An index page again, for DISCOVERY rather than lookup. It was
        // taken out once, with reason: a reader looking a recipe up has this
        // sidebar, and a map page in front of it restated the menu they were
        // already reading. So the section opened on its first chapter, the
        // Cheat Sheet - and a first visit landed on a page whose opening line
        // says it is a checklist, not an introduction. The index is that
        // introduction: one row per group with what it covers and where it
        // starts, and the sheet named as the page to keep open. Lookup still
        // goes straight to the rows below; the row above them opens the map.
        text: "Cookbook",
        link: "/cookbook/",
        collapsed: true,
        items: [
          // The sheet you keep open while working - first, because this is
          // the section a reader who is looking something up opens, and the
          // sheet is the shortest answer to most of what they look up. It
          // stood under Tutorial for a while; its URL always said cookbook,
          // and the tutorial's index links it.
          { text: "Cheat Sheet", link: "/cookbook/cheat_sheet" },
          {
            text: "View",
            link: "/cookbook/view/definition",
            collapsed: true,
            items: [
              { text: "Definition", link: "/cookbook/view/definition" },
              { text: "Nested Views", link: "/cookbook/view/nested_views" },
              { text: "XML Templating", link: "/cookbook/view/xml_templating" },
            ],
          },
          {
            text: "Model",
            link: "/cookbook/model/binding",
            collapsed: true,
            items: [
              {
                text: "Binding",
                link: "/cookbook/model/binding",
                items: [
                  { text: "Expression Binding", link: "/cookbook/model/expression_binding" },
                  { text: "Formatter", link: "/cookbook/model/formatter" },
                ],
              },
              { text: "Tables", link: "/cookbook/model/tables" },
              { text: "Trees", link: "/cookbook/model/trees" },
              { text: "Device Model", link: "/cookbook/model/device_model" },
              { text: "Size Limit", link: "/cookbook/model/size_limit" },
            ],
          },
          {
            text: "Event, Navigation",
            link: "/cookbook/event_navigation/life_cycle",
            collapsed: true,
            items: [
              { text: "Life Cycle", link: "/cookbook/event_navigation/life_cycle" },
              {
                text: "Event",
                link: "/cookbook/event_navigation/backend",
                items: [
                  { text: "Backend", link: "/cookbook/event_navigation/backend" },
                  { text: "Frontend", link: "/cookbook/event_navigation/frontend" },
                ],
              },
              {
                text: "Navigation",
                link: "/cookbook/event_navigation/navigation/inner_app",
                items: [
                  { text: "Inner App", link: "/cookbook/event_navigation/navigation/inner_app" },
                  { text: "Cross App", link: "/cookbook/event_navigation/navigation/cross_app" },
                  { text: "Hash", link: "/cookbook/event_navigation/navigation/hash" },
                  { text: "App State, Share, Bookmark", link: "/cookbook/event_navigation/navigation/app_state" },
                ],
              },
              { text: "Exception", link: "/cookbook/event_navigation/exception" },
            ],
          },
          {
            text: "Popup, Popover",
            link: "/cookbook/popup_popover/popup",
            collapsed: true,
            items: [
              { text: "Popup", link: "/cookbook/popup_popover/popup" },
              { text: "Popover", link: "/cookbook/popup_popover/popover" },
            ],
          },
          {
            text: "Translation, Messages",
            link: "/cookbook/translation_messages/message",
            collapsed: true,
            items: [
              { text: "Message", link: "/cookbook/translation_messages/message" },
              { text: "Logging", link: "/cookbook/translation_messages/logging" },
              { text: "Translation", link: "/cookbook/translation_messages/translation_i18n" },
            ],
          },
          {
            text: "Browser Interaction",
            link: "/cookbook/browser_interaction/title",
            collapsed: true,
            items: [
              { text: "Title", link: "/cookbook/browser_interaction/title" },
              { text: "Focus", link: "/cookbook/browser_interaction/focus" },
              { text: "Scrolling", link: "/cookbook/browser_interaction/scrolling" },
              { text: "Timer", link: "/cookbook/browser_interaction/timer" },
              { text: "Clipboard", link: "/cookbook/browser_interaction/clipboard" },
              { text: "URL Handling", link: "/cookbook/browser_interaction/url_handling" },
              { text: "Soft Keyboard", link: "/cookbook/browser_interaction/soft_keyboard" },
              { text: "Keyboard Shortcuts", link: "/cookbook/browser_interaction/keyboard_shortcuts" },
            ],
          },
          {
            text: "Device Capabilities",
            link: "/cookbook/device_capabilities/info",
            collapsed: true,
            items: [
              { text: "Info", link: "/cookbook/device_capabilities/info" },
              { text: "Camera", link: "/cookbook/device_capabilities/camera" },
              {
                text: "Geolocation",
                link: "/cookbook/device_capabilities/geolocation",
              },
              {
                text: "Barcode Scanning",
                link: "/cookbook/device_capabilities/barcode_scanning",
              },
              { text: "Audio, Video", link: "/cookbook/device_capabilities/audio" },
              {
                text: "Upload, Download",
                link: "/cookbook/device_capabilities/upload_download",
                items: [
                  { text: "PDF", link: "/cookbook/device_capabilities/pdf" },
                  { text: "Spreadsheet", link: "/cookbook/device_capabilities/spreadsheet" },
                ],
              },
            ],
          },
          {
            text: "State, Connectivity",
            link: "/cookbook/expert_more/lock",
            collapsed: true,
            items: [
              { text: "Lock", link: "/cookbook/expert_more/lock" },
              { text: "Statefulness", link: "/cookbook/expert_more/statefulness" },
              { text: "WebSocket", link: "/cookbook/expert_more/websocket" },
              { text: "OData", link: "/cookbook/expert_more/odata" },
              { text: "Smart Controls", link: "/cookbook/expert_more/smart_controls" },
            ],
          },
          // Three chapters that sat behind a collapsible called "More Topics"
          // until now. The name said nothing, and the click it cost fell on
          // Troubleshooting - one of the most-searched pages here, three
          // levels deep behind a label that did not name it. Deprecations
          // left with the group: it is one page and it has a home under
          // Resources, and an entry standing twice in one sidebar makes its
          // own search results ambiguous.
          {
            text: "EML, CDS, SQL",
            link: "/cookbook/eml_cds_sql/rap",
            collapsed: true,
            items: [
              { text: "RAP", link: "/cookbook/eml_cds_sql/rap" },
              { text: "EML", link: "/cookbook/eml_cds_sql/eml" },
              { text: "Draft Handling", link: "/cookbook/eml_cds_sql/draft_handling" },
              { text: "CDS", link: "/cookbook/eml_cds_sql/cds" },
              { text: "ABAP SQL", link: "/cookbook/eml_cds_sql/abap_sql" },
            ],
          },
          {
            text: "Patterns, Helpers",
            collapsed: true,
            items: [
              { text: "Snippets", link: "/cookbook/expert_more/snippets" },
              { text: "Value Help", link: "/cookbook/expert_more/value_help" },
              { text: "Demo Output", link: "/cookbook/expert_more/demo_output" },
              { text: "E-Mail", link: "/cookbook/expert_more/email" },
              { text: "Fuzzy Search", link: "/cookbook/eml_cds_sql/fuzzy_search" },
            ],
          },
          // Troubleshooting is one page now that Debugging stands where its
          // URL says it lives (Configuration), so the page stands in the row
          // rather than alone in a group of one.
          { text: "Common Failures", link: "/cookbook/troubleshooting/common_failures" },
        ],
      },
      {
        // A SECTION OPENS ON ITS FIRST ENTRY. This one opened on Setup while
        // Installation stood first in the list, Advanced opened on
        // Downporting, and a reader who pressed a heading landed somewhere
        // the list under it did not say. Every section link is now the first
        // page under it, and test/sidebar-titles.test.mjs holds it.
        text: "Configuration",
        link: "/configuration/installation",
        collapsed: true,
        items: [
          // The file keeps its name - the URL has been public for years - and
          // the page says what it is about: not how to install the framework
          // (that is the Quickstart) but where the endpoint lives per system.
          { text: "Deployment Scenarios", link: "/configuration/installation" },
          {
            text: "Setup",
            link: "/configuration/setup",
            collapsed: true,
            items: [
              { text: "Theme", link: "/configuration/setup/theme" },
              { text: "Bootstrapping", link: "/configuration/setup/ui5_bootstrapping" },
              { text: "Bootstrap Attributes", link: "/configuration/setup/bootstrap_attributes" },
              { text: "Style / CSS", link: "/configuration/setup/style_css" },
              { text: "Logon Language", link: "/configuration/setup/logon_language" },
            ],
          },
          { text: "Security", link: "/configuration/security" },
          { text: "Authorization", link: "/configuration/authorization" },
          { text: "Performance", link: "/configuration/performance" },
          // What the installation logs about its apps: the roundtrip monitor
          // seam and the Admin Cockpit add-on built on it. Next to
          // Performance because a slow p95 is where a reader of one page
          // goes looking for the other.
          { text: "Monitoring", link: "/configuration/monitoring" },
          { text: "UI5 Versions", link: "/configuration/ui5_versions" },
          { text: "Productive Usage", link: "/configuration/productive_usage" },
          // The operations pages behind it, in the order a Basis team meets
          // them: what a release is and how to move, what travels, what to
          // watch, how much it costs in work processes and table rows.
          { text: "Versioning, Upgrade", link: "/configuration/versioning" },
          { text: "Transport, Landscape", link: "/configuration/transport" },
          { text: "Basis Runbook", link: "/configuration/runbook" },
          { text: "Sizing", link: "/configuration/sizing" },
          { text: "Logout", link: "/configuration/logout" },
          { text: "Debugging", link: "/configuration/debugging" },
          // The Launchpad, Work Zone and Mobile Start moved to Integration,
          // where the other places an app can appear are; their URLs stayed.
          // What was left of "ABAP Cloud, BTP" was this one page, so it
          // stands in the row rather than in a group of one.
          { text: "S/4 Public Cloud", link: "/configuration/s4_public_cloud" },
        ],
      },
      {
        text: "Advanced Topics",
        // The section's own page: one row per group below, with what it
        // covers and its first page. It opened on User Exits, the first leaf
        // of the first group, which told a reader nothing about the other
        // four groups.
        link: "/advanced/",
        collapsed: true,
        items: [
          // Twelve entries in a flat list until now, of four different kinds:
          // what you can build with it, where you can run it, what it talks
          // to, and what you extend. Grouped by that, the section is seven
          // entries and Technical Insight - the last of them - is visible
          // without scrolling, which it was not when it was the twelfth.
          //
          // Use Cases is gone from this section: it was converted into the
          // Insights under Resources - the clean core levels and the
          // wrapper, on-stack against side-by-side, and one app serving many
          // systems - and the page it used to be is in the git history.
          //
          // Add-ons stood next to it and is now under Resources. Every other
          // page in this section teaches something you do to your own app -
          // downport it, run it off-stack, extend it, wire it to something.
          // Add-ons teaches nothing: it is a list of what other people
          // published, so it reads as a resource. The pages that need it still
          // link it by name.
          {
            text: "Extensibility",
            collapsed: true,
            items: [
              {
                text: "User Exits",
                link: "/advanced/extensibility/user_exits",
              },
              { text: "Frontend", link: "/advanced/extensibility/frontend" },
              {
                text: "Custom Controls",
                link: "/advanced/extensibility/custom_control",
              },
            ],
          },
          {
            // EVERY PLACE AN APP CAN APPEAR outside its own browser tab, in
            // one group: the SAP entry points, the phone, Microsoft 365, other
            // systems and AI agents. They stood in three places once - the
            // Launchpad, Work Zone and Mobile Start under Configuration, the
            // connectors and the embed control under Advanced Topics - then in
            // a top-level section of their own, and now in this group. The
            // pages kept their URLs; only the menu moved. The group opens on its
            // own overview, which is that list.
            text: "Integration",
            link: "/integration/",
            collapsed: true,
            items: [
              {
                text: "SAP",
                collapsed: true,
                items: [
                  { text: "Fiori Launchpad", link: "/configuration/launchpad" },
                  { text: "Build Work Zone", link: "/configuration/btp" },
                  { text: "Fiori Elements Integration", link: "/advanced/fiori" },
                ],
              },
              {
                text: "Mobile",
                collapsed: true,
                items: [
                  { text: "Mobile Start", link: "/configuration/mobile_start" },
                  { text: "Native Mobile Shell", link: "/integration/mobile_shell" },
                ],
              },
              {
                text: "Microsoft 365",
                collapsed: true,
                items: [
                  { text: "Microsoft Teams", link: "/integration/teams" },
                  { text: "Microsoft Excel", link: "/integration/excel" },
                ],
              },
              {
                // What the app talks to on the outside: another ABAP system, and
                // an AI agent over MCP - in the sandbox, from VS Code, or as an
                // endpoint in the system.
                text: "Systems, Agents",
                collapsed: true,
                items: [
                  { text: "RFC Connector", link: "/advanced/rfc" },
                  { text: "HTTP Connector", link: "/advanced/http" },
                  { text: "Agent-Operable Apps", link: "/advanced/agents" },
                ],
              },
            ],
          },
          {
            // Where it runs: an older release, a renamed namespace, a system
            // the framework is not installed on, a UI5 runtime without the
            // legacy libraries. Every one of them is the same question -
            // will this work on MY system - asked about a different axis.
            text: "Releases, Stacks",
            collapsed: true,
            items: [
              { text: "Downporting", link: "/advanced/downporting" },
              { text: "Namespaces, Renaming", link: "/advanced/renaming" },
              { text: "Working Off-Stack", link: "/advanced/working_off_stack" },
              // A classic report taken to ABAP Cloud as a class of the
              // abap-cloud-gui add-on - another stack the code was not
              // written for, which is what this group is about.
              { text: "Migrating Classic Reports", link: "/advanced/report_migration" },
              // its counterpart for new code: an app written as a report
              { text: "Apps Written Like Reports", link: "/advanced/report_style" },
              { text: "UI5 Legacy-Free", link: "/advanced/legacy_free" },
            ],
          },
          {
            // The machine you develop on, and the tools the project ships
            // for it - Tooling walks the whole loop in order, from the
            // playground to the agent runtime, and the three with a page of
            // their own are documented in full here, so their READMEs can
            // stay short and point at a page instead of growing a second
            // copy that drifts. Tooling moved here from Getting Started
            // (which editor, transpiler and client tools you develop WITH is
            // a question a newcomer does not have yet), and its file moved
            // with it, so the crumb line and the URL say the same thing.
            text: "Developer Setup",
            collapsed: true,
            items: [
              { text: "Tooling", link: "/advanced/tooling" },
              { text: "npm Packages", link: "/advanced/npm_packages" },
              { text: "abap2UI5 linter", link: "/advanced/linter" },
              { text: "MCP Server", link: "/advanced/mcp_server" },
              { text: "VS Code Extension", link: "/advanced/vscode" },
              { text: "Agent Setup", link: "/advanced/agent_setup" },
            ],
          },
          {
            // Every one of these is somebody else's project, which is what
            // this group is: the toolchain abap2UI5 stands on. Four have a
            // page of their own; abap-cleaner, open-abap, abapmerge and
            // abap2UI5-local were one paragraph each and are sections of the
            // overview, which is also where the group opens.
            text: "Toolchain",
            link: "/technical/tools/",
            collapsed: true,
            items: [
              { text: "abapGit", link: "/technical/tools/abapgit" },
              { text: "ajson", link: "/technical/tools/ajson" },
              { text: "S-RTTI", link: "/technical/tools/srtti" },
              { text: "abaplint", link: "/technical/tools/abaplint" },
            ],
          },
        ],
      },
      {
        text: "Resources",
        link: "/resources/api",
        collapsed: true,
        items: [
          // Generated from z2ui5_if_client at the pinned release by
          // scripts/generate-api-reference.mjs — the entry lives here rather
          // than in the Cookbook because it is a lookup destination, not a
          // reading path: the cookbook chapters explain, this page lists.
          { text: "Client API", link: "/resources/api" },
          // No "Sample Catalogues" entry here any more. A page that only
          // described the three catalogues put a stop between the reader and
          // the corpus, and had to be kept true about counts and facets it
          // did not own. The catalogue pages introduce themselves and link
          // one another; the home page opens the first of them directly, and
          // the cookbook links individual samples per chapter.
          //
          // Moved here from Advanced Topics: the ecosystem around the
          // framework rather than a technique to apply to your own app, and
          // it closes by asking the reader to add their project.
          { text: "Add-ons", link: "/resources/addons" },
          { text: "Release Notes", link: "/resources/changelog" },
          { text: "Deprecations", link: "/resources/deprecations" },
          {
            // The series of essays, moved here from a top-level "Know-How"
            // section of its own: a reader looks for them among the resources,
            // as reading about the framework rather than a step in using it.
            // The URLs stay where they were published.
            text: "Insights",
            link: "/advanced/insights/",
            collapsed: true,
            items: [
              // Four blocks - why, how it works, a working day, where it
              // belongs - and the index page at the link above names them. The
              // numbers are part of each title on purpose: the articles are
              // posted one at a time, and "#21" is how a reader refers to one.
              { text: "#1 Somewhere on the Way to UI5, We Lost RTTS", link: "/advanced/insights/01-somewhere-on-the-way-to-ui5" },
              { text: "#2 The Cost of a Screen", link: "/advanced/insights/02-the-cost-of-a-screen" },
              { text: "#3 One Class, No Programming Model", link: "/advanced/insights/03-one-class-no-programming-model" },
              { text: "#4 No Annotation in Between", link: "/advanced/insights/04-no-annotation-in-between" },
              { text: "#5 UI5 Over-the-Wire", link: "/advanced/insights/05-ui5-over-the-wire" },
              { text: "#6 The Frontend Knows Nothing", link: "/advanced/insights/06-the-frontend-knows-nothing" },
              { text: "#7 One Handler for Every App", link: "/advanced/insights/07-one-handler-for-every-app" },
              { text: "#8 Only the Changed Part", link: "/advanced/insights/08-only-the-changed-part" },
              { text: "#9 A New Instance on Every Request", link: "/advanced/insights/09-a-new-instance-on-every-request" },
              { text: "#10 Swapping the View at Runtime", link: "/advanced/insights/10-swapping-the-view-at-runtime" },
              { text: "#11 No Build, No Deploy, No Cache", link: "/advanced/insights/11-no-build-no-deploy-no-cache" },
              { text: "#12 Where Your Own JavaScript Goes", link: "/advanced/insights/12-where-your-own-javascript-goes" },
              { text: "#13 Four Verbs, Every Control", link: "/advanced/insights/13-four-verbs-every-control" },
              { text: "#14 A Classrun for the Browser", link: "/advanced/insights/14-a-classrun-for-the-browser" },
              { text: "#15 Where the Selection Screen Went", link: "/advanced/insights/15-where-the-selection-screen-went" },
              { text: "#16 One Click, One Request", link: "/advanced/insights/16-one-click-one-request" },
              { text: "#17 What the Client Can Do", link: "/advanced/insights/17-what-the-client-can-do" },
              { text: "#18 CALL SCREEN, LEAVE SCREEN", link: "/advanced/insights/18-call-screen-leave-screen" },
              { text: "#19 Where F4 Went", link: "/advanced/insights/19-where-f4-went" },
              { text: "#20 MESSAGE Still Works", link: "/advanced/insights/20-message-still-works" },
              { text: "#21 The Lock Is Gone by the Next Click", link: "/advanced/insights/21-the-lock-is-gone-by-the-next-click" },
              { text: "#22 Who May Start Which App", link: "/advanced/insights/22-who-may-start-which-app" },
              { text: "#23 100 Rows, Four Hours, One Request", link: "/advanced/insights/23-100-rows-four-hours-one-request" },
              { text: "#24 ABAP Unit for a Screen", link: "/advanced/insights/24-abap-unit-for-a-screen" },
              { text: "#25 When One Class Is Not Enough", link: "/advanced/insights/25-when-one-class-is-not-enough" },
              { text: "#26 A Tile Like Any Other", link: "/advanced/insights/26-a-tile-like-any-other" },
              { text: "#27 One Codebase, 7.02 to ABAP Cloud", link: "/advanced/insights/27-one-codebase-702-to-abap-cloud" },
              { text: "#28 Cloud-Ready Is a Property of Your App", link: "/advanced/insights/28-cloud-ready-is-a-property-of-your-app" },
              { text: "#29 When the API Is Not Released", link: "/advanced/insights/29-when-the-api-is-not-released" },
              { text: "#30 On-Stack or Side-by-Side", link: "/advanced/insights/30-on-stack-or-side-by-side" },
              { text: "#31 One App, Many Systems", link: "/advanced/insights/31-one-app-many-systems" },
              { text: "#32 From ITS to abap2UI5", link: "/advanced/insights/32-from-its-to-abap2ui5" },
              { text: "#33 RAP or abap2UI5?", link: "/advanced/insights/33-rap-or-abap2ui5" },
              { text: "#34 UI5 Freestyle or abap2UI5?", link: "/advanced/insights/34-freestyle-or-abap2ui5" },
                  { text: "#36 Written for Agents", link: "/advanced/insights/36-written-for-agents" },
              { text: "#37 What the Client Can Send", link: "/advanced/insights/37-what-the-client-can-send" },
              { text: "#38 How Many Users", link: "/advanced/insights/38-how-many-users" },
              { text: "#39 Upgrading the Framework", link: "/advanced/insights/39-upgrading-the-framework" },
            ],
          },
          {
            // The project rather than the framework: what it costs, who to
            // ask, how to join in. Six entries that stood between the
            // reading destinations above and pushed them off the first
            // screen - and every one of them is already reachable twice
            // over, from the footer and the version menu. Collapsed, at the
            // bottom, where a reader goes looking for them on purpose.
            text: "Project",
            collapsed: true,
            items: [
              { text: "License", link: "/resources/license" },
              { text: "Support", link: "/resources/support" },
              // SIDEBAR copy — the nav bar has the same two entries verbatim,
              // further up under the version number. Search for this marker,
              // not for the text.
              { text: "Contribution", link: "/resources/contribution" }, // sidebar
              { text: "Sponsor", link: "/resources/sponsor" }, // sidebar
              // The logo, the favicon and the cover image, for anyone writing
              // about abap2UI5. The page existed and no sidebar linked it, so
              // the only way in was knowing the URL - which nobody looking for
              // a logo does. Found by scripts/generate-llms.mjs, which reports
              // a page in the tree that no sidebar navigates to.
              { text: "Logo & Brand", link: "/resources/logo" },
            ],
          },
        ],
      },
    ],
    outline: [2, 6],
    // Inline SVG rather than a named icon: a named one VitePress may fetch
    // from a CDN.
    socialLinks: [
      { icon: { svg: '<svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg>' }, link: "https://github.com/abap2UI5/abap2UI5", ariaLabel: "abap2UI5 on GitHub" },
      { icon: { svg: '<svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><path d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433c-1.144 0-2.063-.926-2.063-2.065 0-1.138.92-2.063 2.063-2.063 1.14 0 2.064.925 2.064 2.063 0 1.139-.925 2.065-2.064 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.225 0z"/></svg>' }, link: "https://www.linkedin.com/company/abap2ui5/", ariaLabel: "abap2UI5 on LinkedIn" },
    ],
  },
});
