defmodule Ash.Authorization.Check.AttributeEquals do
  use Ash.Authorization.Check, action_types: [:read, :update, :delete]

  @impl true
  def describe(opts) do
    "this_record.#{opts[:field]} == #{inspect(opts[:value])}"
  end

  @impl true
  def strict_check(_user, request, options) do
    field = options[:field]
    value = options[:value]

    case Ash.Filter.parse(request.resource, [{field, eq: value}]) do
      %{errors: []} = parsed ->
        if Ash.Filter.strict_subset_of?(parsed, request.filter) do
          {:ok, true}
        else
          case Ash.Filter.parse(request.resource, [{field, not_eq: value}]) do
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
  def check(_user, records, _state, options) do
    matches =
      Enum.filter(records, fn record ->
        Map.fetch(record, options[:field]) == {:ok, options[:value]}
      end)

    {:ok, matches}
  end
end
