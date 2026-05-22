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
          { text: "Cookbook", link: "/development/controller/life_cycle" },
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
          { text: "Use Case", link: "/get_started/use_cases" },
          { text: `What's Next?`, link: "/get_started/next" },
        ],
      },
      {
        text: "Cookbook",
        link: "/development/controller/life_cycle",
        collapsed: true,
        items: [
          {
            text: "Controller",
            link: "/development/controller/life_cycle",
            collapsed: true,
            items: [
              { text: "Life Cycle", link: "/development/controller/life_cycle" },
              { text: "Init", link: "/development/controller/init" },
              { text: "Event", link: "/development/controller/event" },
              { text: "Navigated", link: "/development/controller/navigated" },
            ],
          },
          {
            text: "View",
            link: "/development/view/overview",
            collapsed: true,
            items: [
              { text: "Overview", link: "/development/view/overview" },
              { text: "Definition", link: "/development/view/definition" },
              { text: "Nested Views", link: "/development/view/nested" },
              { text: "XML Templating", link: "/development/specific/xml_templating" },
            ],
          },
          {
            text: "Model",
            link: "/development/model/binding",
            collapsed: true,
            items: [
              {
                text: "Binding",
                link: "/development/model/binding",
                items: [
                  { text: "Expression Binding", link: "/development/model/expression_binding" },
                  { text: "Formatter", link: "/development/specific/formatter" },
                ],
              },
              { text: "Tables", link: "/development/model/tables" },
              { text: "Trees", link: "/development/model/trees" },
              { text: "Device Model", link: "/development/model/device" },
              { text: "Size Limit", link: "/development/model/size_limit" },
            ],
          },
          {
            text: "Event, Navigation",
            link: "/development/events/backend",
            collapsed: true,
            items: [
              { text: "Backend", link: "/development/events/backend" },
              { text: "Frontend", link: "/development/events/frontend" },
              { text: "Follow-up", link: "/development/events/follow_up" },
              { text: "Navigation", link: "/development/navigation/inner_cross_app" },
            ],
          },
          {
            text: "Popup, Popover",
            link: "/development/popups/popup",
            collapsed: true,
            items: [
              { text: "Popup", link: "/development/popups/popup" },
              { text: "Popover", link: "/development/popups/popover" },
              { text: "Built-In", link: "/development/popups/built_in" },
            ],
          },
          {
            text: "Device Capabilities",
            link: "/development/specific/barcodes",
            collapsed: true,
            items: [
              {
                text: "Barcode Scanning",
                link: "/development/specific/barcodes",
              },
              { text: "Camera", link: "/development/specific/camera" },
              {
                text: "Geolocation",
                link: "/development/specific/geolocation",
              },
              { text: "Clipboard", link: "/development/specific/clipboard" },
              {
                text: "Upload, Download",
                link: "/development/specific/files",
                items: [
                  { text: "PDF", link: "/development/specific/pdf" },
                  { text: "Spreadsheet", link: "/development/specific/xlsx" },
                ],
              },
            ],
          },
          {
            text: "UI Interaction",
            link: "/development/specific/focus",
            collapsed: true,
            items: [
              { text: "Focus", link: "/development/specific/focus" },
              { text: "Scrolling", link: "/development/specific/scrolling" },
              { text: "Soft Keyboard", link: "/development/specific/soft_keyboard" },
              { text: "Title", link: "/development/specific/title" },
              { text: "Timer", link: "/development/specific/timer" },
              { text: "URL Handling", link: "/development/specific/url" },
            ],
          },
          {
            text: "Message, Error",
            link: "/development/messages/messages",
            collapsed: true,
            items: [
              { text: "Message", link: "/development/messages/messages" },
              { text: "Error", link: "/development/messages/errors" },
              { text: "Logging", link: "/development/messages/logging" },
              { text: "Translation, i18n", link: "/development/translation" },
            ],
          },
          {
            text: "EML, CDS, SQL",
            link: "/development/specific/rap",
            collapsed: true,
            items: [
              { text: "RAP", link: "/development/specific/rap" },
              { text: "EML", link: "/development/specific/eml" },
              { text: "Draft Handling", link: "/development/specific/draft" },
              { text: "CDS", link: "/development/specific/cds" },
              { text: "ABAP SQL", link: "/development/specific/abap_sql" },
              { text: "Fuzzy Search", link: "/development/specific/fuzzy_search" },
            ],
          },
          {
            text: "Expert, More",
            link: "/development/specific/locks",
            collapsed: true,
            items: [
              { text: "Lock", link: "/development/specific/locks" },
              { text: "Statefulness", link: "/development/specific/statefulness" },
              { text: "WebSocket", link: "/development/specific/websocket" },
              { text: "Logout", link: "/configuration/logout" },
              { text: "OData", link: "/development/model/odata" },
              { text: "App State", link: "/development/navigation/app_state" },
              { text: "Share, Bookmark", link: "/development/navigation/share" },
              {
                text: "Utilities",
                collapsed: true,
                items: [
                  { text: "Value Help", link: "/development/specific/value_help" },
                  { text: "Demo Output", link: "/development/specific/demo_output" },
                  { text: "E-Mail", link: "/development/specific/email" },
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
              { text: "Custom JavaScript", link: "/advanced/extensibility/custom_js" },
              { text: "Frontend", link: "/advanced/extensibility/frontend" },
              {
                text: "Custom Control",
                link: "/advanced/extensibility/custom_control",
              },
            ],
          },
          {
            text: "Experimental",
            collapsed: true,
            items: [
              { text: "Drag & Drop", link: "/development/specific/drag" },
              {
                text: "Smart Control",
                link: "/development/specific/smart_controls",
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
