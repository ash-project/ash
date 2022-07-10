      # taken from `attribute.always_select?`
      Useful if fields are used in read action preparations consistently.

      A primary key attribute *cannot be deselected*, so this option will have no effect.

      Generally, you should favor selecting the field that you need while running your preparation. For example:

      ```elixir
      defmodule MyApp.QueryPreparation.Thing do
        use Ash.Resource.Preparation

        def prepare(query, _, _) do
          query
          # we use `ensure_selected` here because we don't want
          # to limit the fields being selected to only this attribute, rather
          # rather we want to make sure this field is definitely selected.
          |> Ash.Query.ensure_selected(:attribute_i_need)
          |> Ash.Query.after_action(fn query, results ->
            {:ok, Enum.map(results, fn result ->
              do_something_with_attribute_i_need(result)
            end)}
          end)
        end
      end
      ```

      This will prevent unnecessary fields from being selected.


# Taken from `attribute.generated?`
If it is, the data layer will know to read the value back after writing.