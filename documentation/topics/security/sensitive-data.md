# Sensitive Data

## Public & Private Attributes

By default, attributes, calculations, aggregates and relationships are _private_ (they are marked `public?: false`).  
If you are working with Ash in code, reading a resource, for example using `Ash.read/2`, the public/private status of an attribute is not relevant.  
However, when working with api extensions like `AshGraphql` and `AshJsonApi`, they will only include public fields in their interfaces. This helps avoid accidentally exposing data over "public" interfaces.

## Public & Private Arguments

Public/private arguments work the same way as public/private fields, except that they default to `public?: true`.  
This is because arguments to an action being used in a public interface would naturally be expected to be `public`. If an argument is marked as `public?: false`, it can only be set with `Ash.Query.set_argument/3` or `Ash.Changeset.set_argument/3`

## Sensitive Attributes

Using `sensitive? true` will cause an attribute, calculation or argument to show as `"** Redacted **"` when inspecting records.  
In filter statements, any value used in the same expression as a sensitive field will also be redacted. For example, you might see: `email == "** Redacted **"` in a filter statement if `email` is marked as sensitive.

### Show Sensitive Attributes

**IMPORTANT WARNING:** The following configuration should only ever be used in development mode!

To display sensitive attributes in their original form during development, use the following config.

```elixir
config :ash, show_sensitive?: true
```

## Field Policies

Field policies are a way to control the visibility of individual fields (except for relationships) as a part of authorization flow, for those using `Ash.Policy.Authorizer`.  
If a field is not visible, it will be populated with `%Ash.ForbiddenField{}`, or will be not shown (or may show an error) in public interfaces. See the [Policies guide](documentation/topics/security/policies.md#field-policies) for more.
