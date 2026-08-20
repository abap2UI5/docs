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
          { text: "Introduction", link: "/get_started/about" },
          { text: "Cookbook", link: "/cookbook/overview" },
          { text: "Configuration", link: "/configuration/setup" },
          { text: "Advanced Topic", link: "/advanced/downporting" },
          { text: "Technical Insight", link: "/technical/concept" },
          { text: "Resource", link: "/resources/references" },
        ],
      },
      {
        // Just the repositories, flat. The dropdown used to carry the three
        // sample catalogues on top of them, which meant nine entries in two
        // groups and a reader scanning for "where is the code" reading past
        // half of it first. The catalogues are a reading destination and are
        // linked where a reader looks for one — /resources/samples in the
        // sidebar, and the cookbook chapters; this menu answers the other
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
          { text: "Release", link: "/resources/changelog" },
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
          { text: "Introduction", link: "/get_started/about" },
          {
            text: "Quickstart",
            items: [
              { text: "Installation", link: "/get_started/quickstart" },
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
                  { text: "Expression", link: "/cookbook/model/expression_binding" },
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
              { text: "Action", link: "/cookbook/event_navigation/action" },
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
          {
            text: "More Topics",
            link: "/cookbook/eml_cds_sql/rap",
            collapsed: true,
            items: [
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
              {
                text: "Obsolete",
                collapsed: true,
                items: [
                  { text: "Deprecations", link: "/resources/deprecations" },
                ],
              },
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
          { text: "UI5 Version", link: "/configuration/ui5_versions" },
          { text: "Production Use", link: "/configuration/productive_usage" },
          { text: "Transport", link: "/configuration/transport" },
          { text: "Launchpad", link: "/configuration/launchpad" },
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
        text: "Advanced Topic",
        link: "/advanced/downporting",
        collapsed: true,
        items: [
          // Both moved out of Getting Started on purpose: the extensibility
          // tiers and the git/CI project setup answer questions a newcomer
          // does not have yet, and on the entry pages they read as required
          // steps. Here they sit with the other depth topics.
          { text: "Use Cases", link: "/advanced/use_cases" },
          { text: "Add-ons", link: "/advanced/addons" },
          { text: "Downporting", link: "/advanced/downporting" },
          { text: "Namespaces, Renaming", link: "/advanced/renaming" },
          { text: "Working Off-Stack", link: "/advanced/working_off_stack" },
          {
            // The project's own tools, each documented in full here — this is
            // the documentation for those three repositories, so their READMEs
            // can stay short and point at a page instead of growing a second
            // copy that drifts.
            text: "Tools",
            link: "/advanced/linter",
            collapsed: true,
            items: [
              // ADVANCED copy — Technical Insight > Tool carries the same
              // entry, pointing at the same page. Match on the marker.
              { text: "abap2UI5 linter", link: "/advanced/linter" }, // advanced
              { text: "MCP Server", link: "/advanced/mcp_server" },
              { text: "VS Code Extension", link: "/advanced/vscode" },
            ],
          },
          { text: "Local Setup", link: "/advanced/local" },
          { text: "RFC Connector", link: "/advanced/rfc" },
          { text: "HTTP Connector", link: "/advanced/http" },
          { text: "Fiori Elements Integration", link: "/advanced/fiori" },
          { text: "UI5 Legacy-Free", link: "/advanced/legacy_free" },
          {
            text: "Extensibility",
            collapsed: true,
            items: [
              {
                text: "User Exit",
                link: "/advanced/extensibility/user_exits",
              },
              { text: "Frontend", link: "/advanced/extensibility/frontend" },
              {
                text: "Custom Control",
                link: "/advanced/extensibility/custom_control",
              },
            ],
          },
        ],
      },
      {
        text: "Technical Insight",
        link: "/technical/concept",
        collapsed: true,
        items: [
          { text: "UI5 Over-the-Wire", link: "/technical/concept" },
          { text: "ABAP Thinking, UI5 Result", link: "/technical/dx" },
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
            text: "Tool",
            collapsed: true,
            items: [
              // The project's own linter, next to the tools it borrows. Every
              // other gate in this section is somebody else's; this one is
              // the only thing that can read a view that does not exist until
              // the app runs.
              //
              // TECHNICAL copy — the page itself lives under Advanced Topic >
              // Tools with the MCP server and the extension, the project's
              // other two. Match on the marker, not on the text.
              { text: "abap2UI5 linter", link: "/advanced/linter" }, // technical
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
      {
        text: "Resource",
        link: "/resources/references",
        collapsed: true,
        items: [
          { text: "Reference", link: "/resources/references" },
          // Several hundred working apps in three repositories, and until this
          // page the only way to find out which one to open was to know all
          // three existed. The cookbook links individual samples per chapter;
          // this says which CATALOGUE answers which question. The figures live
          // on the page itself, where check:counts verifies them against the
          // catalogues - do not repeat one here, where nothing would.
          { text: "Sample Catalogues", link: "/resources/samples" },
          { text: "Who Uses abap2UI5?", link: "/resources/who_uses" },
          { text: "Release", link: "/resources/changelog" },
          { text: "Deprecations", link: "/resources/deprecations" },
          { text: "License", link: "/resources/license" },
          { text: "Support", link: "/resources/support" },
          { text: "Contact", link: "/resources/contact" },
          // SIDEBAR copy — the nav bar has the same two entries verbatim,
          // further up under the version number. Search for this marker, not
          // for the text.
          { text: "Contribution", link: "/resources/contribution" }, // sidebar
          { text: "Sponsor", link: "/resources/sponsor" }, // sidebar
          // The logo, the favicon and the cover image, for anyone writing
          // about abap2UI5. The page existed and no sidebar linked it, so the
          // only way in was knowing the URL - which nobody looking for a logo
          // does. Found by scripts/generate-llms.mjs, which reports a page in
          // the tree that no sidebar navigates to.
          { text: "Logo, Press Kit", link: "/resources/logo" },
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
