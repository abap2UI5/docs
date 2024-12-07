# Designtime, Runtime


abap2UI5 dies not persist any retracts of the app or model at deisgntime. It solely at runtime check switch UI and model is used and bereites es auf and sends it to the frontend. The frontend service is generic and can dynamically send anything to the frontend. Nothing is hardly typed.

UI -> OData -> CDS View -> RAP
UI -> OData -> CDS View -> RAP
UI -> OData -> CDS View -> RAÜ

UI
UI  REST -> abap2UI5 -> app, app, app
UI


 service 