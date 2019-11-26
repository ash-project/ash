# Ash

**TODO: Add description**

## TODO LIST (in no order)

* Add central API configuration DSL `api do ... end`
  * Use this for `default_page_size` and `max_page_size`
* Make our router cabaple of describing its routes in `mix phx.routes` Chris McCord says that we could probably power that, seeing as phoenix controls both APIs, and that capability could be added to `Plug.Router`
* Finish the serializer
* Make primary key type configurable
* Make a DSL for join tables to support complex validation/hooks into how they work, support more than just table names in `join_through`
* DSL level validations! Things like includes validating that their chain exists. All DSL structs should be strictly validated when they are created.
* Especially at compile time, we should *never* ignore or skip invalid options. If an option is present and invalid, an error is raised.
* break up the `Ash` module
* Wire up/formalize the error handling
* Ensure that errors are properly propagated up from the data_layer behaviour, and every operation is allowed to fail
* figure out the ecto schema warning
* all actions need to be performed in a transaction
* document authorization thoroughly. *batch* (default) checks need to return a list of `ids` for which the check passed.
* So many parts of the system are reliant on things having an `id` key explicitly. THis will need to be addressed some day, and will be a huge pain in the ass
* Validate that the user resource has a get action
* The fact that the code level interface shouldn't be authorization is a potentially good reason to split the code level interface off of the interface that an API frontend my call into. Specifically, the grossness of it is highlighted by how we default `user` when handling actions to `:__none__`. This is because a client must be able to set the user to `nil` or a value, but the code lever interface should just skip validation (which is what `:__none__` does)
