defmodule Ash.Error.Framework.ManualActionMissed do
  @moduledoc "Used when a manual action should have produced a result, but did not"
  use Ash.Error.Exception

  def_ash_error([:resource, :action, :type], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "manual_action_missed"

    def message(%{resource: resource, action: action, type: :create}) do
      """
      Manual action did not produce a result: #{inspect(resource)}.#{action}

      For manual actions, you must implement an `after_action` inside of a `change` that returns a newly created record.

      For example:

      # in the resource

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
      """
    end
  end
end
