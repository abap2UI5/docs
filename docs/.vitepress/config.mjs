import { defineConfig } from "vitepress";

// https://vitepress.dev/reference/site-config
export default defineConfig({
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
    }]
  ],
  title: "abap2UI5",
  description: "Build UI5 Apps Purely in ABAP",
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
          { text: "Cookbook", link: "/cookbook/event_navigation/life_cycle" },
          { text: "Configuration", link: "/configuration/setup" },
          { text: "Advanced Topic", link: "/advanced/downporting" },
          { text: "Technical Insight", link: "/technical/concept" },
          { text: "Resource", link: "/resources/addons" },
        ],
      },
      {
        text: "Links",
        items: [
          {
            text: "Project",
            items: [
              {
                text: "abap2UI5",
                link: "https://github.com/abap2UI5/abap2UI5",
              },
              { text: "addons", link: "https://github.com/abap2UI5-addons" },
              { text: "samples", link: "https://github.com/abap2UI5/samples" },
              { text: "docs", link: "https://github.com/abap2UI5/docs" },
              {
                text: "issues",
                link: "https://github.com/abap2UI5/abap2UI5/issues",
              },
            ],
          },
        ],
      },
      {
        text: "1.141.0",
        items: [
          { text: "Release", link: "/resources/changelog" },
          { text: "Support", link: "/resources/support" },
          { text: "Contribution", link: "/resources/contribution" },
          { text: "Sponsor", link: "/resources/sponsor" },
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
          { text: "Quickstart", link: "/get_started/quickstart" },
          { text: "Hello World", link: "/get_started/hello_world" },
          { text: "Sample App", link: "/get_started/samples" },
          { text: "Use Cases", link: "/get_started/use_cases" },
          { text: `What's Next?`, link: "/get_started/next" },
        ],
      },
      {
        text: "Cookbook",
        link: "/cookbook/event_navigation/life_cycle",
        collapsed: true,
        items: [
          {
            text: "View",
            link: "/cookbook/view/overview",
            collapsed: true,
            items: [
              { text: "Overview", link: "/cookbook/view/overview" },
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
              { text: "Action", link: "/cookbook/event_navigation/action" },
              { text: "Navigation", link: "/cookbook/event_navigation/navigation" },
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
              { text: "Translation, i18n", link: "/cookbook/translation_messages/translation_i18n" },
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
              { text: "Soft Keyboard", link: "/cookbook/device_capabilities/soft_keyboard" },
              {
                text: "Barcode Scanning",
                link: "/cookbook/device_capabilities/barcode_scanning",
              },
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
            text: "Beyond Basics",
            link: "/cookbook/expert_more/lock",
            collapsed: true,
            items: [
              {
                text: "Advanced Features",
                collapsed: true,
                items: [
                  { text: "Lock", link: "/cookbook/expert_more/lock" },
                  { text: "Statefulness", link: "/cookbook/expert_more/statefulness" },
                  { text: "WebSocket", link: "/cookbook/expert_more/websocket" },
                  { text: "Logout", link: "/configuration/logout" },
                  { text: "OData", link: "/cookbook/expert_more/odata" },
                  { text: "App State, Share", link: "/cookbook/expert_more/app_state_share" },
                ],
              },
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
                text: "Obsolete",
                collapsed: true,
                items: [
                  { text: "Custom Controls", link: "/cookbook/expert_more/custom_controls" },
                  { text: "Custom JS", link: "/cookbook/expert_more/custom_js" },
                  { text: "follow_up_action", link: "/cookbook/expert_more/follow_up_action" },
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
          { text: "General", link: "/configuration/setup" },
          { text: "Security", link: "/configuration/security" },
          { text: "Authorization", link: "/configuration/authorization" },
          { text: "Performance", link: "/configuration/performance" },
          { text: "UI5 Version", link: "/configuration/ui5_versions" },
          { text: "Production Use", link: "/configuration/productive_usage" },
          { text: "Debugging", link: "/configuration/troubleshooting" },
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
          { text: "Downporting", link: "/advanced/downporting" },
          { text: "Renaming", link: "/advanced/renaming" },
          { text: "Build Process", link: "/advanced/builds" },
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
        link: "/resources/addons",
        collapsed: true,
        items: [
          { text: "Add-on", link: "/resources/addons" },
          { text: "Reference", link: "/resources/references" },
          { text: "Who Uses abap2UI5?", link: "/resources/who_uses" },
          { text: "Release", link: "/resources/changelog" },
          { text: "License", link: "/resources/license" },
          { text: "Support", link: "/resources/support" },
          { text: "Contact", link: "/resources/contact" },
          { text: "Contribution", link: "/resources/contribution" },
          { text: "Sponsor", link: "/resources/sponsor" },
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
