

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