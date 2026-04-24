---
outline: [2, 4]
---
# Statefulness, Locks

By default, abap2UI5 runs stateless like any other UI5 freestyle app, making only REST calls to the ABAP backend.

#### Stateful Sessions

For private and on-premise systems, you can also run abap2UI5 in stateful mode. See `Z2UI5_CL_DEMO_APP_135` and `Z2UI5_CL_DEMO_APP_137` for examples.

#### Locks

You may need to lock business objects. Stateful sessions (described above) are one way.

#### Infinite Transactions

Another option: create an infinite session in the backend that holds locks while the UI5 app still communicates statelessly. See the [SAP documentation on infinite sessions](https://help.sap.com/docs/ABAP_PLATFORM_NEW/6568469cf5a1460a8d85c58b83d21ec2/47db6c68e4282972e10000000a42189b.html?locale=en-US) and discussions in [issue #2003](https://github.com/abap2UI5/abap2UI5/issues/2003) and [issue #1971](https://github.com/abap2UI5/abap2UI5/issues/1971) for more details.
