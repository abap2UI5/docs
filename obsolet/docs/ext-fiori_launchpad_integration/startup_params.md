# Setup Startup Parameters & Title

### Add Parameter to the Target Mapping
Transaction /UI2/FLPD_CUST <br>
Go to the Target Mapping of your tile and add an additional Parameter:
![image](https://github.com/abap2UI5/abap2UI5-documentation/assets/102328295/c1383769-34ab-4be0-af6b-21bf2f2ddbba)
<br>
If you want to customize a parameter with a namespace eg. /YYY/MY_VAL use -YYY-MY_VAL instead, abap2UI5 will map the values accordingly.

### Read the Parameters in your App Implementation
Use the following code to read the value in ABAP:
```abap
    DATA(lt_params) = client->get( )-t_comp_params.
```
Check out sample: z2ui5_cl_demo_app_187 


### Set a Title
You can set the launchpad title in the view definiiton of your app:
```abap
      page->_z2ui5( )->lp_title( client->_bind_edit( mv_title ) ).
```

Check out sample: z2ui5_cl_demo_app_188
