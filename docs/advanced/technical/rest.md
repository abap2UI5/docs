# Draft, REST

gives you the freedom in designing you data and UI at designative but also at runtime. The  you r

In abap2UI5, you don’t necessarily need to type your data model at design time, just work with generic data references and apply typing at runtime based on your program logic. This is for example useful for tables where, depending on user input, you may want to display different columns.
abap2UI5 serializes your app instances to ensure stateless behavior in client communication. Unfortunately, the SAP standard serialization features are limited; for example, they do not support local types created at runtime. If you encounter problems, just install the fantastic project S-RTTI filling this gap.
Basic

Fully typed data at design time, it works out of the box: