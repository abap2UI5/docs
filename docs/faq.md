# FAQ

## Setup
#### **1. How to change the theme?**
Change the HTTP handler of abap2UI5 to:
```abap
    DATA(lt_config) = VALUE z2ui5_if_types=>ty_t_name_value(
          (  n = `data-sap-ui-theme`         v = `sap_belize` ) "<- adjusted
          (  n = `src`                       v = `https://sdk.openui5.org/resources/sap-ui-core.js` )
          (  n = `data-sap-ui-libs`          v = `sap.m` )
          (  n = `data-sap-ui-bindingSyntax` v = `complex` )
          (  n = `data-sap-ui-frameOptions`  v = `trusted` )
          (  n = `data-sap-ui-compatVersion` v = `edge` ) ).

    DATA(response) = z2ui5_cl_http_handler=>main(
                       body   = server->request->get_cdata( )
                       config = VALUE #( t_config = lt_config ) ).

    server->response->set_cdata( response ).
    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_status( code = 200 reason = `success` ).
```
#### **2. How to change the bootstrapping of UI5?**
Change the HTTP handler of abap2UI5 to:
```abap
DATA(lt_config) = VALUE z2ui5_if_types=>ty_t_name_value( 
      (  n = `data-sap-ui-theme`         v = `sap_belize` ) 
      (  n = `src`                       v = `https://ui5.sap.com/1.116.0/resources/sap-ui-core.js` ) "<- adjusted ( for example to load from sap cdn)
      (  n = `data-sap-ui-libs`          v = `sap.m` )
      (  n = `data-sap-ui-bindingSyntax` v = `complex` )
      (  n = `data-sap-ui-frameOptions`  v = `trusted` )
      (  n = `data-sap-ui-compatVersion` v = `edge` ) ).

    DATA(response) = z2ui5_cl_http_handler=>main(
                       body   = server->request->get_cdata( )
                       config = VALUE #( t_config = lt_config ) ).

    server->response->set_cdata( response ).
    server->response->set_header_field( name = `cache-control` value = `no-cache` ).
    server->response->set_status( code = 200 reason = `success` ).
```

## Client & UI

#### **1. How to read URL parameters?**
Use the following snippet:
```abap
DATA(lv_search) = client->get( )-s_config-search.
DATA(lv_param) = z2ui5_cl_xml_view=>factory( client )->hlp_get_url_param( `myparam` ).
```
#### **2. How to format numbers, times and dates?**
Take a look at the following example: <br>
https://github.com/abap2UI5/demo-demos/blob/main/src/z2ui5_cl_app_demo_47.clas.abap
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5/assets/102328295/6b9011b5-94e6-4329-9666-0e779c01b400">

#### **3. How to format the output of currencies?**
Take a look at the following example: <br>
https://github.com/abap2UI5/demo-demos/blob/main/src/z2ui5_cl_app_demo_67.clas.abap
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5/assets/102328295/fef6e08c-5a34-4aee-9d34-ebb1c5d45275">

#### **4. How to call an url in a new tab?**
Use the following snippet:
```abap
client->timer_set(
      interval_ms    = 0
      event_finished = client->_event_client( action = client->cs_event-open_new_tab t_arg = value #( ( `https://www.github.com/abap2UI5` )  )
) ).
```
#### 5. **Which values can be send back via client->_event( t_arg = value # ( ( ... ?**
Check the documentation [here.](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe) There are different ways for adressing the event handler ($event, $source, $params) and you can select your value for example with /mProperties/property.<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/6de59970-f766-46fd-a35a-933d26287564">
<br>
You can also call functions directly in the view as explained [here.](https://sapui5.hana.ondemand.com/#/entity/sap.m.ActionSheet/sample/sap.m.sample.ActionSheet/code/view/ActionSheet.fragment.xml) <br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/17650cca-d84a-4e88-be2b-244ead773b52">
<br>
#### 5. **How can I change the logon language?**
Set the url parameter sap-language for this and check all other options [here.](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)

## Extension
#### 1. **How can i call my own custom function?**
Extend the initial HTTP Get call:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/b671b89f-530b-4a35-98ba-e1a779fc0087"><br>
And in your application, call the function in the view definition:<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/2f041184-a8c1-4d11-893a-ee6800b3214c">


## Productive Usage
#### **1. Can abap2UI5 used in a productive system?**
The project is technically just an implementation of an HTTP handler and can be used as any other HTTP Service also in a productive scenario.
#### **2. Are there any dependencies or preparations needed before using abap2UI5 in a productiv scenario?**
No, but it is recommended to follow these steps before using abap2UI5 apps in a productive scenario:
1. Transport the abap2UI5 HTTP service and the framework first
2. Sometimes an extra activation of the HTTP service is needed, along with an adjustment of the UI5 bootstrapping
3. Test the "hello world" app to ensure that abap2UI5 works correctly
4. Now continue with tranporting your custom apps...
#### **3. Does a stable version of abap2UI5 exist?**
The project will be continuously further developed. Therefore, there is no specific "stable" version. However, adjustments to the public APIs will be kept to a minimum to avoid frequent refactoring of apps. You can use [releases](https://github.com/abap2ui5/abap2ui5/releases/) instead of pulling the main branch and only update from time to time to reduce refactoring efforts.
