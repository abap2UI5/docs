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
  head: [
    ["link", { rel: "shortcut icon", href: "/docs/favicon.ico" }],
    [
      "link",
      { rel: "apple-touch-icon", sizes: "180x180", href: "/docs/favicon.ico" },
    ],
    [
      "link",
      {
        rel: "stylesheet",
        href: "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css",
      },
    ],
    // No webfonts. Montserrat and Fira Code were requested here and applied
    // by nothing — no rule in theme/style.css, or anywhere else, ever named
    // either one — so the site paid for two render-blocking stylesheets and
    // four font files it never showed a glyph from. The type is now the
    // system stack, set in theme/style.css. Font Awesome below STAYS: the
    // three home-page feature icons are `fa-solid`/`fa-brands` classes.
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
    logo: "/logo.png",
    footer: {
      message: `
      <a href="/docs/resources/license">License</a> |
      <a href="/docs/resources/contact">Contact</a>`,
      copyright: `Copyright © 2023-${new Date().getFullYear()} abap2UI5`,
    },
    editLink: {
      pattern:
        "https://github.com/abap2UI5/docs/tree/main/docs/:path",
      text: "Edit this page on GitHub",
    },
    search: {
      provider: "local",
    },
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      {
        text: "Guide",
        items: [
          // The section is called Getting Started in the sidebar this opens;
          // "Introduction" was the first PAGE in it, one level down.
          { text: "Getting Started", link: "/get_started/about" }, // nav
          { text: "Tutorial", link: "/tutorials/walkthrough/" }, // nav
          { text: "Cookbook", link: "/cookbook/view/definition" },
          { text: "Configuration", link: "/configuration/setup" },
          // Technical Insight is not a line of its own here any more: it is a
          // subsection of Advanced Topic in the sidebar, and a flat dropdown
          // that still lists it next to its parent tells the reader a
          // different structure than the sidebar they land in.
          { text: "Advanced Topics", link: "/advanced/downporting" }, // nav
          { text: "Resources", link: "/resources/references" }, // nav
        ],
      },
      {
        // Just the repositories, flat. The dropdown used to carry the three
        // sample catalogues on top of them, which meant nine entries in two
        // groups and a reader scanning for "where is the code" reading past
        // half of it first. The catalogues are a reading destination and are
        // linked where a reader looks for one — the button under the home
        // page grid, and the cookbook chapters; this menu answers the other
        // question, which repository to clone.
        text: "Links",
        items: [
          { text: "abap2UI5", link: "https://github.com/abap2UI5/abap2UI5" },
          { text: "addons", link: "https://github.com/abap2UI5-addons" },
          { text: "samples", link: "https://github.com/abap2UI5/samples" },
          {
            text: "samples-controls",
            link: "https://github.com/abap2UI5/samples-controls",
          },
          {
            text: "samples-stack",
            link: "https://github.com/abap2UI5/samples-stack",
          },
          { text: "docs", link: "https://github.com/abap2UI5/docs" },
          { text: "issues", link: "https://github.com/abap2UI5/abap2UI5/issues" },
        ],
      },
      {
        // the released framework version — z2ui5_if_app=>version in
        // abap2UI5/src/02/z2ui5_if_app.intf.abap is where it comes from
        text: "1.144.0",
        items: [
          { text: "Release Notes", link: "/resources/changelog" },
          { text: "Support", link: "/resources/support" },
          // NAV copy — the sidebar has the same two entries verbatim, further
          // down under "Resource". Search for this marker, not for the text.
          { text: "Contribution", link: "/resources/contribution" }, // nav
          { text: "Sponsor", link: "/resources/sponsor" }, // nav
        ],
      },
    ],
    sidebar: [
      {
        text: "Getting Started",
        link: "/get_started/about",
        collapsed: true,
        items: [
          { text: "In a Nutshell", link: "/get_started/about" }, // sidebar
          {
            // Install it, and see one app run. The step-by-step tutorials -
            // Full Example among them - are a section of their own now; what
            // stays here is the shortest path from nothing to a running app.
            text: "Quickstart",
            items: [
              { text: "Install with abapGit", link: "/get_started/quickstart" },
              { text: "Hello World", link: "/get_started/hello_world" },
            ],
          },
          { text: "Developing with AI", link: "/get_started/ai" },
          { text: `What's Next?`, link: "/get_started/next" },
        ],
      },
      {
        // Between Getting Started and the Cookbook on purpose: a tutorial is
        // read after the framework is installed and before the reference
        // chapters, and that is the order the three sections stand in.
        text: "Tutorial",
        link: "/tutorials/walkthrough/",
        collapsed: true,
        items: [
          {
            // No `collapsed` key at all, like Quickstart above: that is what
            // makes a group a plain labelled list instead of a collapsible
            // one. `collapsed: false` would still render the toggle and still
            // let the steps be folded away - and the sequence IS the tutorial,
            // so a reader working through it has to see where they are on
            // every page.
            //
            // The group label itself opens the walkthrough's index page.
            // That page used to stand above the group as a separate
            // "Overview" entry, which put two lines in front of the reader
            // for one page - the label they were looking for and the entry
            // that actually opened it.
            text: "Walkthrough",
            link: "/tutorials/walkthrough/",
            items: [
              { text: "1. The App Class", link: "/tutorials/walkthrough/step-1" },
              { text: "2. A First View", link: "/tutorials/walkthrough/step-2" },
              { text: "3. Events", link: "/tutorials/walkthrough/step-3" },
              { text: "4. Data Binding", link: "/tutorials/walkthrough/step-4" },
              { text: "5. List Binding", link: "/tutorials/walkthrough/step-5" },
              { text: "6. Row Events", link: "/tutorials/walkthrough/step-6" },
              { text: "7. Popups", link: "/tutorials/walkthrough/step-7" },
              { text: "8. Selection Screen", link: "/tutorials/walkthrough/step-8" },
              { text: "9. Tables", link: "/tutorials/walkthrough/step-9" },
              { text: "10. App Structure", link: "/tutorials/walkthrough/step-10" },
              { text: "11. To Production", link: "/tutorials/walkthrough/step-11" },
              { text: "12. Unit Tests", link: "/tutorials/walkthrough/step-12" },
            ],
          },
          // The sheet you keep open WHILE working through the steps. It keeps
          // the URL it was published under - only the entry that navigates to
          // it moved out of the Cookbook, and it is not listed twice: an entry
          // standing in two sidebar sections makes its own search results
          // ambiguous.
          { text: "Cheat Sheet", link: "/cookbook/cheat_sheet" },
        ],
      },
      {
        // No overview page any more. This is a collection of concrete
        // problem-and-solution chapters, and a map page in front of it was a
        // stop between the reader and the recipe - it restated the sidebar
        // they were already looking at. The section opens on the first
        // chapter instead.
        text: "Cookbook",
        link: "/cookbook/view/definition",
        collapsed: true,
        items: [
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
              { text: "Logout", link: "/configuration/logout" },
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
          {
            text: "Troubleshooting",
            collapsed: true,
            items: [
              { text: "Debugging", link: "/configuration/debugging" },
              { text: "Common Failures", link: "/cookbook/troubleshooting/common_failures" },
            ],
          },
        ],
      },
      {
        text: "Configuration",
        link: "/configuration/setup",
        collapsed: true,
        items: [
          { text: "Installation", link: "/configuration/installation" },
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
          { text: "UI5 Versions", link: "/configuration/ui5_versions" },
          { text: "Production Use", link: "/configuration/productive_usage" },
          { text: "Transport", link: "/configuration/transport" },
          { text: "Fiori Launchpad", link: "/configuration/launchpad" },
          {
            text: "ABAP Cloud, BTP",
            collapsed: true,
            items: [
              {
                text: "S/4 Public Cloud",
                link: "/configuration/s4_public_cloud",
              },
              { text: "BTP ABAP Environment", link: "/configuration/btp_abap_env" },
              { text: "Build Work Zone", link: "/configuration/btp" },
              { text: "Mobile Start", link: "/configuration/mobile_start" },
            ],
          },
        ],
      },
      {
        text: "Advanced Topics",
        link: "/advanced/downporting",
        collapsed: true,
        items: [
          // Twelve entries in a flat list until now, of four different kinds:
          // what you can build with it, where you can run it, what it talks
          // to, and what you extend. Grouped by that, the section is seven
          // entries and Technical Insight - the last of them - is visible
          // without scrolling, which it was not when it was the twelfth.
          //
          // Use Cases and Add-ons stay loose at the top: both moved out of
          // Getting Started on purpose, because they answer questions a
          // newcomer does not have yet, and on the entry pages they read as
          // required steps. They are the two that say what this section is
          // for, so they are what a reader opening it meets first.
          { text: "Use Cases", link: "/advanced/use_cases" },
          { text: "Add-ons", link: "/advanced/addons" },
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
            // What the app talks to on the outside. Three pages that were
            // three unrelated-looking lines in the flat list.
            text: "Integration",
            collapsed: true,
            items: [
              { text: "RFC Connector", link: "/advanced/rfc" },
              { text: "HTTP Connector", link: "/advanced/http" },
              { text: "Fiori Elements Integration", link: "/advanced/fiori" },
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
              { text: "UI5 Legacy-Free", link: "/advanced/legacy_free" },
            ],
          },
          {
            // The machine you develop on, and the three tools the project
            // ships for it - each documented in full here, so their READMEs
            // can stay short and point at a page instead of growing a second
            // copy that drifts.
            text: "Developer Setup",
            collapsed: true,
            items: [
              // Moved out of Getting Started: which editor, transpiler and
              // client tools you develop WITH is a question a newcomer does
              // not have yet, and on the entry path it read as a required
              // step. It belongs next to the local setup it describes.
              { text: "Tooling", link: "/get_started/tooling" },
              { text: "Local Setup", link: "/advanced/local" },
              {
                text: "Project Tools",
                link: "/advanced/linter",
                collapsed: true,
                items: [
                  { text: "abap2UI5 linter", link: "/advanced/linter" },
                  { text: "MCP Server", link: "/advanced/mcp_server" },
                  { text: "VS Code Extension", link: "/advanced/vscode" },
                ],
              },
            ],
          },
          {
            // Every one of these is somebody else's project, which is what
            // this group is: the toolchain abap2UI5 stands on. It used to be
            // the last entry INSIDE Technical Insight, one level further
            // down, where a reader looking for "how does abapGit fit in" had
            // to open a group about the framework's internals first. Next to
            // Developer Setup - the tools you develop WITH - and Technical
            // Insight - what the framework does under the app - it is one of
            // the things this section is for, and reachable in one click.
            //
            // The project's own linter used to head the list, pointing at
            // the same page as Developer Setup > Project Tools above. One
            // entry standing twice in one sidebar makes its own search
            // results ambiguous, and here it also made the group claim
            // something it no longer holds.
            text: "Toolchain",
            collapsed: true,
            items: [
              { text: "abapGit", link: "/technical/tools/abapgit" },
              { text: "ajson", link: "/technical/tools/ajson" },
              { text: "S-RTTI", link: "/technical/tools/srtti" },
              { text: "abaplint", link: "/technical/tools/abaplint" },
              { text: "open-abap", link: "/technical/tools/open_abap" },
              { text: "abap-cleaner", link: "/technical/tools/abap_cleaner" },
              { text: "abapmerge", link: "/technical/tools/abapmerge" },
            ],
          },
          {
            // The article series. It replaced eight narrative pages that used
            // to stand here as "Technical Insight" - the same reading, cut
            // into one claim per article so each can be read on its own and
            // posted on its own. Last in this section on purpose: it is the
            // part read after the app runs, not on the way in, and a reader
            // who opens Advanced Topics for a how-to should meet the how-tos
            // first.
            text: "Technical Insights",
            link: "/advanced/insights/01-somewhere-on-the-way-to-ui5",
            collapsed: true,
            items: [
              { text: "Somewhere on the Way to UI5, We Lost RTTS", link: "/advanced/insights/01-somewhere-on-the-way-to-ui5" },
              { text: "abap2UI5 is not a Programming Model", link: "/advanced/insights/02-not-a-programming-model" },
              { text: "The Cost of a Screen", link: "/advanced/insights/03-the-cost-of-a-screen" },
              { text: "No Annotation in Between", link: "/advanced/insights/04-no-annotation-in-between" },
              { text: "UI5 Over-the-Wire", link: "/advanced/insights/05-ui5-over-the-wire" },
              { text: "The Frontend That Does Not Know What It Shows", link: "/advanced/insights/06-the-frontend-that-does-not-know" },
              { text: "One Service for Every App", link: "/advanced/insights/07-one-service-for-every-app" },
              { text: "Only the Changed Part", link: "/advanced/insights/08-only-the-changed-part" },
              { text: "PUBLIC Means Persisted", link: "/advanced/insights/09-public-means-persisted" },
              { text: "Swapping the View at Runtime", link: "/advanced/insights/10-swapping-the-view-at-runtime" },
              { text: "index.html Lives in a String", link: "/advanced/insights/11-index-html-lives-in-a-string" },
              { text: "Where Your Own JavaScript Goes", link: "/advanced/insights/12-where-your-own-javascript-goes" },
              { text: "Four Verbs", link: "/advanced/insights/13-four-verbs" },
              { text: "The Class That Runs", link: "/advanced/insights/14-the-class-that-runs" },
              { text: "Where the Selection Screen Went", link: "/advanced/insights/15-where-the-selection-screen-went" },
              { text: "No Cache, No Deploy, Any IDE", link: "/advanced/insights/16-no-cache-no-deploy-any-ide" },
              { text: "One Codebase, 7.02 to ABAP Cloud", link: "/advanced/insights/17-one-codebase-702-to-abap-cloud" },
              { text: "2,300 Lines", link: "/advanced/insights/18-2300-lines" },
              { text: "Where the Line Is", link: "/advanced/insights/19-where-the-line-is" },
              { text: "Cloud-Ready Is a Property of Your App", link: "/advanced/insights/20-cloud-ready-is-a-property-of-your-app" },
              { text: "Twenty-Five Years of ABAP on the Web", link: "/advanced/insights/21-abap-on-the-web" },
              { text: "Where the View Lives", link: "/advanced/insights/22-where-the-view-lives" },
              { text: "RAP or abap2UI5 — When to Use Which", link: "/advanced/insights/23-rap-or-abap2ui5" },
              { text: "UI5 Freestyle or abap2UI5 — When to Use Which", link: "/advanced/insights/24-freestyle-or-abap2ui5" },
              { text: "Low-Code or abap2UI5 — When to Use Which", link: "/advanced/insights/25-low-code-or-abap2ui5" },
            ],
          },
        ],
      },
      {
        text: "Resources",
        link: "/resources/references",
        collapsed: true,
        items: [
          { text: "References", link: "/resources/references" },
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
          { text: "Who Uses abap2UI5?", link: "/resources/who_uses" },
          { text: "Release Notes", link: "/resources/changelog" },
          { text: "Deprecations", link: "/resources/deprecations" },
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
              { text: "Contact", link: "/resources/contact" },
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
              { text: "Logo, Press Kit", link: "/resources/logo" },
            ],
          },
        ],
      },
    ],
    outline: [2, 6],
    socialLinks: [
      { icon: "linkedin", link: "https://www.linkedin.com/company/abap2ui5/" },
      { icon: "github", link: "https://github.com/abap2UI5/abap2UI5" },
    ],
  },
});
