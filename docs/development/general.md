---
outline: [2, 4]
---
# General


#### Event

`client->_event( t_arg = value # ( ( ...`

Check the documentation [here.](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe) There are different ways for adressing the event handler ($event, $source, $params) and you can select your value for example with /mProperties/property.<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/6de59970-f766-46fd-a35a-933d26287564">
<br>
You can also call functions directly in the view as explained [here.](https://sapui5.hana.ondemand.com/#/entity/sap.m.ActionSheet/sample/sap.m.sample.ActionSheet/code/view/ActionSheet.fragment.xml) <br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/17650cca-d84a-4e88-be2b-244ead773b52">
<br>


#### **URL Parameters**
Use the following snippet:
```abap
DATA(lv_search) = client->get( )-s_config-search.
DATA(lv_param) = z2ui5_cl_xml_view=>factory( client )->hlp_get_url_param( `myparam` ).
```
#### **Format numbers, times & dates**
Take a look at the following example: <br>
https://github.com/abap2UI5/demo-demos/blob/main/src/z2ui5_cl_app_demo_47.clas.abap
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5/assets/102328295/6b9011b5-94e6-4329-9666-0e779c01b400">

#### **Format Currencies**
Take a look at the following example: <br>
https://github.com/abap2UI5/demo-demos/blob/main/src/z2ui5_cl_app_demo_67.clas.abap
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5/assets/102328295/fef6e08c-5a34-4aee-9d34-ebb1c5d45275">

#### **Open new tab**
Use the following snippet:
```abap
client->timer_set(
      interval_ms    = 0
      event_finished = client->_event_client( action = client->cs_event-open_new_tab t_arg = value #( ( `https://www.github.com/abap2UI5` )  )
) ).
```

#### Logon Language
Set the url parameter sap-language for this and check all other options [here.](https://help.sap.com/doc/saphelp_nw75/7.5.5/de-DE/8b/46468c433b40c3b87b2e07f34dea1b/content.htm?no_cache=true)


