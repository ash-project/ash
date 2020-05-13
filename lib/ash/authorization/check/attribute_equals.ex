defmodule Ash.Authorization.Check.AttributeEquals do
  use Ash.Authorization.Check, action_types: [:read, :create, :update, :delete]

  @impl true
  def describe(opts) do
    "this_record.#{opts[:field]} == #{inspect(opts[:value])}"
  end

  @impl true
  def strict_check(_user, request = %{action_type: :read}, options) do
    field = options[:field]
    value = options[:value]

    case Ash.Filter.parse(request.resource, [{field, eq: value}], request.query.api) do
      %{errors: []} = parsed ->
        if Ash.Filter.strict_subset_of?(parsed, request.query.filter) do
          {:ok, true}
        else
          case Ash.Filter.parse(request.resource, [{field, not_eq: value}], request.query.api) do
            %{errors: []} = parsed ->
              if Ash.Filter.strict_subset_of?(parsed, request.query.filter) do
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

  def strict_check(_user, %{action_type: :create, changeset: changeset}, options) do
    case Ecto.Changeset.fetch_field(changeset, options[:field]) do
      {_, value} -> {:ok, options[:value] == value}
      _ -> {:ok, false}
    end
  end

  def strict_check(_user, %{action_type: :update, changeset: changeset}, options) do
    {:ok, {:ok, options[:value]} == Map.fetch(changeset.data, options[:field])}
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
