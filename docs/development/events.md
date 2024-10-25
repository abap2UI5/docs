


#### Event

`client->_event( t_arg = value # ( ( ...`

Check the documentation [here.](https://openui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe) There are different ways for adressing the event handler ($event, $source, $params) and you can select your value for example with /mProperties/property.<br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/6de59970-f766-46fd-a35a-933d26287564">
<br>
You can also call functions directly in the view as explained [here.](https://sapui5.hana.ondemand.com/#/entity/sap.m.ActionSheet/sample/sap.m.sample.ActionSheet/code/view/ActionSheet.fragment.xml) <br>
<img width="500" alt="image" src="https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/17650cca-d84a-4e88-be2b-244ead773b52">
<br>


#### **Open new tab**
Use the following snippet:
```abap
client->timer_set(
      interval_ms    = 0
      event_finished = client->_event_client( action = client->cs_event-open_new_tab t_arg = value #( ( `https://www.github.com/abap2UI5` )  )
) ).
```


# Navigation



### Backend
Navigation
       Backend 
       Frontend 



### Frontend (Launchpad)