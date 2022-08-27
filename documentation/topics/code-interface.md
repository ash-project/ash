

    If the action is an update or destroy, it will take a record or a changeset as its *first* argument.
    If the action is a read action, it will take a starting query as an *opt in the last* argument.

    All functions will have an optional last argument that accepts options. Those options are:

    #{Spark.OptionsHelpers.docs(Ash.Resource.Interface.interface_options(nil))}

    For reads:

    * `:query` - a query to start the action with, can be used to filter/sort the results of the action.

    For creates:

    * `:changeset` - a changeset to start the action with

    They will also have an optional second to last argument that is a freeform map to provide action input. It *must be a map*.
    If it is a keyword list, it will be assumed that it is actually `options` (for convenience).
    This allows for the following behaviour:

    ```elixir
    # Because the 3rd argument is a keyword list, we use it as options
    Api.register_user(username, password, [tenant: "organization_22"])
    # Because the 3rd argument is a keyword list, we use it as action input
    Api.register_user(username, password, %{key: "val"})
    # When all are provided it is unambiguous
    Api.register_user(username, password, %{key: "val"}, [tenant: "organization_22"])
    ```

    ## get?

      Only relevant for read actions. Expects to only receive a single result from a read action.

      The action should return a single result based on any arguments provided. To make it so that the function
      takes a specific field, and filters on that field, use `get_by` instead.

      Useful for creating functions like `get_user_by_email` that map to an action that has an `:email` argument.

    ## get_by

      Automatically sets `get?` to `true`.

      The action should return a single result based on any arguments provided. To make it so that the function
      takes a specific field, and filters on that field, use `get_by` instead. When combined, `get_by` takes precedence.

      Useful for creating functions like `get_user_by_id` that map to a basic read action.

    ## get_by_identity