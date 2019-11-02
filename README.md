# Ash

**TODO: Add description**

## TODO LIST (in no order)

* Add central API configuration DSL `api do ... end`
  * Use this for `default_page_size` and `max_page_size`
* Make our router cabaple of describing its routes in `mix phx.routes` Chris McCord says that we could probably power that, seeing as phoenix controls both APIs, and that capability could be added to `Plug.Router`
* Finish the serializer
* Make primary key type configurable
* Make a DSL for join tables to support complex validation/hooks into how they work, support more than just table names in `join_through`
* DSL level validations! Things like includes validating that their chain exists.
* break up the `Ash` module
* Wire up/formalize the error handling
* Ensure that errors are properly propagated up from the data_layer behaviour, and every operation is allowed to fail
* figure out the ecto schema warning