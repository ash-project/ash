# Ash

**TODO: Add description**

## TODO LIST (in no order)

* Add central API configuration DSL `api do ... end`
  * Use this for `default_page_size` and `max_page_size`
* Figure out if we want to run ecto (I think we do) and if we want to run the repo (I think we don't)
* Make our router cabaple of describing its routes in `mix phx.routes` Chris McCord says that we could probably power that, seeing as phoenix controls both APIs, and that capability could be added to `Plug.Router`
* Finish the serializer
* Make primary key type configurable
* Make a DSL for join tables to support complex validation/hooks into how they work, support more than just table names in `join_through`
* DSL level validations! Things like includes validating that their chain exists.