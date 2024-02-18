defmodule Ash.Test.Flows.CancelParentResource do
  @moduledoc false

  use Ash.Flow

  flow do
    api Ash.Test.Flow.Api

    argument :record, Ash.Test.Flow.ParentResource do
      allow_nil? false
    end

    argument :status, :atom do
      allow_nil? false
    end

    returns :parent_resource
  end

  steps do
    custom :record_id, fn input, _ -> {:ok, input} end do
      input path(arg(:record), [:id])
    end

    branch :maybe_cancel_child_resource, expr(not is_nil(^result(:record_id))) do
      read :child_resources, Ash.Test.Flow.ChildResource, :read do
        not_found_error? false

        input %{
          build: %{
            filter: [parent_resource_id: path(arg(:record), [:id])]
          }
        }
      end

      map :cancel_child_resources, result(:child_resources) do
        update :cancel_child_resource, Ash.Test.Flow.ChildResource, :system_cancel do
          record element(:cancel_child_resources)
        end
      end
    end

    update :parent_resource, Ash.Test.Flow.ParentResource, :system_disable do
      record arg(:record)
      wait_for [result(:cancel_child_resources)]

      input %{
        status: arg(:status)
      }
    end
  end
end

defmodule Ash.Test.Actions.CancelParentResource do
  @moduledoc false

  use Ash.Resource.ManualUpdate

  alias Ash.Test.Flows.CancelParentResource

  @impl true
  def update(%Ash.Changeset{data: record, arguments: %{status: status}}, _opts, _) do
    %Ash.Flow.Result{
      result: result,
      errors: errors,
      valid?: valid?
    } = CancelParentResource.run(record, status)

    case {result, errors, valid?} do
      {resource, [], true} ->
        {:ok, resource}

      {_, errors, _} when is_list(errors) and length(errors) > 0 ->
        {:error, errors}

      {_, _, false} ->
        {:error, "Invalid"}
    end
  end
end
