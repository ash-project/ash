

      Available template functions:

      - `arg/1` to refer to a flow argument
      - `result/1` to refer to the result of another step


    If given a single step, then the result of the step is returned. If given multiple, then a map of step name to result is returned.
    If nothing is provided, then the last step is returned.

    To rename keys in the map of step names to results, use a keyword list, where the key is the step and the value is what should be in
    the returned map.

    For example:

    `returns :step_name`
    `returns [:step_one, :step_two]`
    `returns [step_one: :one, step_two: :two]`


## Custom steps

        Generally speaking, you should also set the `touches_resources` if you set `async?` to true.
        This ensures that the custom step will be run synchronously if any of those resource's data
        layers is in a corresponding transaction. You don't necessarily need to set *all* of the
        resources that will be touched. For example, all AshPostgres resources that share the same
        repo share the same transaction state.