defmodule Ash.Authorization.Check.UserAttributeMatchesRecord do
  use Ash.Authorization.Check, action_types: [:read, :update, :delete]
  alias Ash.Engine2.Request

  @impl true
  def describe(opts) do
    "user.#{opts[:user_field]} == this_record.#{opts[:record_field]}"
  end

  @impl true
  def strict_check(nil, _, _), do: {:ok, false}

  def strict_check(user, request, options) do
    user_field = options[:user_field]
    record_field = options[:record_field]

    value = Map.get(user, user_field)

    case Ash.Filter.parse(request.resource, [{record_field, eq: value}]) do
      %{errors: []} = parsed ->
        if Ash.Filter.strict_subset_of?(parsed, request.filter) do
          {:ok, true}
        else
          case Ash.Filter.parse(request.resource, [{record_field, not_eq: value}]) do
            %{errors: []} = parsed ->
              if Ash.Filter.strict_subset_of?(parsed, request.filter) do
                {:ok, false}
              else
                {:ok, :unknown}
              end
          end
        end

      %{errors: errors} ->
        {:error, errors}
    end
  end

  @impl true
  def check(user, records, _state, options) do
    user_value = Map.fetch(user, options[:user_field])

    matches =
      Enum.filter(records, fn record ->
        user_value == Map.fetch(record, options[:record_field])
      end)

    {:ok, matches}
  end
end
