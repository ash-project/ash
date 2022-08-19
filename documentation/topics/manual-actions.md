

      All validation still takes place, but the `result` in any `after_action` callbacks
      attached to that action will simply be the record that was read from the database initially.
      For creates, the `result` will be `nil`, and you will be expected to handle the changeset in
      an after_action callback and return an instance of the record. This is a good way to prevent
      Ash from issuing an unnecessary update to the record, e.g updating the `updated_at` of the record
      when an action actually only involves modifying relating records.

      You could then handle the changeset automatically.

      For example:

      # in the action

      ```elixir
      action :special_create do
        manual? true
        change MyApp.DoCreate
      end

      # The change
      defmodule MyApp.DoCreate do
        use Ash.Resource.Change

        def change(changeset, _, _) do
          Ash.Changeset.after_action(changeset, fn changeset, _result ->
            # result will be `nil`, because this is a manual action

            result = do_something_that_creates_the_record(changeset)

            {:ok, result}
          end)
        end
      end
      ```

# for manual reads


                    Manual read actions will simply be handed the ash query and the data layer query.
                    If you simply want to customize/intercept the query before it is sent to the data layer
                    then use `modify_query` instead. Using them in conjunction can help ensure that calculations and aggregates
                    are all correct. For example, you could modify the query to alter/replace the where clause/filter using
                    `modify_query` which will affect which records calculations are returned for. Then you can customize how it is
                    run using `manual`.

                    ```elixir
                    # in the resource
                    actions do
                      read :action_name do
                        manual MyApp.ManualRead
                        # or `{MyApp.ManualRead, ...opts}`
                      end
                    end

                    # the implementation
                    defmodule MyApp.ManualRead do
                      use Ash.Resource.ManualRead

                      def read(ash_query, ecto_query, _opts, _context) do
                        ...
                        {:ok, query_results} | {:error, error}
                      end
                    end
                    ```

