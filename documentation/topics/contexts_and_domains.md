# Contexts and Domains

It is suggested that you read a bit on Domain Driven Design before proceeding. If you are using phoenix or are familiar with phoenix contextxs, then this will make sense to you.

In order to support domain driven design, Ash supports defining multiple APIs, each with their own set of resources. It is possible to share a resource between APIs, but this gets untenable very quickly because any resources related to the shared resource must _both_ appear in each API.

An experimental "Delegation" data layer was added to allow you to use other resources in other APIs as the data layer for a resource, but it created a significant amount of complexity in determining data layer behavior. Instead, simply use the same data layer and configuration in both resources.

Things missing to make this work well:

- Define the ecto schema as a separate module (prerequisite for hidden attributes)
- "hidden" attributes - attributes that are defined on the schema but not the Ash struct
- ability to filter on hidden fields in certain places (haven't determined where this needs to happen)
- ability to add a "base_filter" that can leverage hidden attributes
