import { defineConfig } from "vitepress";
import { playgroundButton } from "./playground.mjs";

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
    config: (md) => playgroundButton(md),
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
    // Montserrat
    ['link', { rel: 'preconnect', href: 'https://fonts.googleapis.com' }],
    ['link', { rel: 'preconnect', href: 'https://fonts.gstatic.com', crossorigin: '' }],
    ['link', {
      rel: 'stylesheet',
      href: 'https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700;800&display=swap'
    }],
    // Optional: Fira Code
    ['link', {
      rel: 'stylesheet',
      href: 'https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;500&display=swap'
    }],
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
          { text: "Tutorials", link: "/tutorials/overview" }, // nav
          { text: "Cookbook", link: "/cookbook/overview" },
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
        text: "1.143.0",
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
          { text: "Introduction", link: "/get_started/about" }, // sidebar
          {
            text: "Quickstart",
            items: [
              { text: "Install with abapGit", link: "/get_started/quickstart" },
              { text: "Hello World", link: "/get_started/hello_world" },
              { text: "Full Example", link: "/get_started/full_example" },
            ],
          },
          { text: "Tooling", link: "/get_started/tooling" },
          { text: "Building with AI", link: "/get_started/ai" },
          { text: `What's Next?`, link: "/get_started/next" },
        ],
      },
      {
        // Between Getting Started and the Cookbook on purpose: a tutorial is
        // read after the framework is installed and before the reference
        // chapters, and that is the order the three sections stand in.
        text: "Tutorials",
        link: "/tutorials/overview",
        collapsed: true,
        items: [
          { text: "Overview", link: "/tutorials/overview" }, // sidebar
          {
            text: "Walkthrough",
            link: "/tutorials/walkthrough/overview",
            collapsed: true,
            items: [
              { text: "Introduction", link: "/tutorials/walkthrough/overview" }, // sidebar
              { text: "1 — Hello World", link: "/tutorials/walkthrough/step-1" },
              { text: "2 — A First View", link: "/tutorials/walkthrough/step-2" },
              { text: "3 — Events", link: "/tutorials/walkthrough/step-3" },
              { text: "4 — Data Binding", link: "/tutorials/walkthrough/step-4" },
              { text: "5 — List Binding", link: "/tutorials/walkthrough/step-5" },
              { text: "6 — Row Events", link: "/tutorials/walkthrough/step-6" },
              { text: "7 — Popups", link: "/tutorials/walkthrough/step-7" },
              { text: "8 — App Structure", link: "/tutorials/walkthrough/step-8" },
            ],
          },
        ],
      },
      {
        text: "Cookbook",
        link: "/cookbook/overview",
        collapsed: true,
        items: [
          { text: "Overview", link: "/cookbook/overview" },
          { text: "Cheat Sheet", link: "/cookbook/cheat_sheet" },
          {
            text: "View",
            link: "/cookbook/view/definition",
            collapsed: true,
            items: [
              { text: "Definition", link: "/cookbook/view/definition" },
              { text: "Deprecated Controls", link: "/cookbook/view/deprecated_controls" },
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
              { text: "Action (Obsolete)", link: "/cookbook/event_navigation/action" },
              { text: "Follow-up Action", link: "/cookbook/expert_more/follow_up_action" },
              { text: "Navigation", link: "/cookbook/event_navigation/navigation" },
              { text: "Routing", link: "/cookbook/event_navigation/routing" },
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
              { text: "Built-In", link: "/cookbook/popup_popover/built_in" },
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
              { text: "App State, Share", link: "/cookbook/expert_more/app_state_share" },
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
              { text: "Favicon", link: "/configuration/setup/favicon" },
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
          // A section of its own until now, next to Advanced Topics rather
          // than inside it. It is the same kind of reading - what the
          // framework does under the app, and the tools it stands on - and
          // it is read after the app runs, not on the way in, so it sits
          // here as the last entry instead of as a second top-level heading
          // competing with this one.
          {
            text: "Technical Insight",
            link: "/technical/concept",
            collapsed: true,
            items: [
              { text: "UI5 Over-the-Wire", link: "/technical/concept" },
              { text: "ABAP Thinking, UI5 Results", link: "/technical/dx" },
              { text: "Cloud Readiness", link: "/technical/cloud" },
              { text: "Behind the Scenes", link: "/technical/how_it_all_works" },
              {
                text: "Technology",
                link: "/technical/technology/overview",
                collapsed: true,
                items: [
                  { text: "RAP", link: "/technical/technology/rap" },
                  { text: "UI5 Freestyle", link: "/technical/technology/ui5" },
                ],
              },
              {
                text: "Toolchain",
                collapsed: true,
                items: [
                  // Every one of these is somebody else's project, which is
                  // what this group is: the toolchain abap2UI5 stands on.
                  // The project's own linter used to head the list, pointing
                  // at the same page as Developer Setup > Project Tools two
                  // groups up. One entry standing twice in one sidebar makes
                  // its own search results ambiguous, and here it also made
                  // the group claim something it no longer holds.
                  { text: "abapGit", link: "/technical/tools/abapgit" },
                  { text: "ajson", link: "/technical/tools/ajson" },
                  { text: "S-RTTI", link: "/technical/tools/srtti" },
                  { text: "abaplint", link: "/technical/tools/abaplint" },
                  { text: "open-abap", link: "/technical/tools/open_abap" },
                  { text: "abap-cleaner", link: "/technical/tools/abap_cleaner" },
                  { text: "abapmerge", link: "/technical/tools/abapmerge" },
                ],
              },
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
