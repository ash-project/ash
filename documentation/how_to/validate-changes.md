## Validations Section

The validations section allows you to create validations based on the changeset.
The only information available is the changeset. If you want to adjust the behavior based
on other details of the request, like the current user, you are most likely looking for
authorization.

A validation is a module that implements the `Ash.Resource.Validation` behaviour. The built in validations
expose utility functions that are imported into the resource's scope, to make them easier to read. You
can do this with custom validations as well. See the documentation in `Ash.Resource.Validation` for more information.
Right now, there are not very many built in validations, but the idea is that eventually we will have a rich
library of built in validations to choose from.

Validations can be scoped to the `type` (`:create`, `:update`, `:destroy`) of action (but not to specific actions). If you would like to adjust the validations for a specific action, you can place that validation directly in the action, i.e

```elixir
create :create do
  validate attribute_equals(:name, "fred")
end
```

### Important Note

By default, validations in the global `validations` block will run on create and update only. Many validations don't make sense in the context of destroys. To make them run on destroy, use `on: [:create, :update, :destroy]`

### Examples

```elixir
validations do
  validate present([:foo, :bar]), on: :update
  validate present([:foo, :bar, :baz], at_least: 2), on: :create
  validate absent([:foo, :bar, :baz], exactly: 1), on: [:update, :destroy]
  validate {MyCustomValidation, [foo: :bar]}, on: :create
end
```
