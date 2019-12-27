defmodule Ash.Authorization.Check.UserAttributeMatchesRecord do
  use Ash.Authorization.Check

  def user_attribute_matches_record(user_field, record_field) do
    {__MODULE__, user_field: user_field, record_field: record_field}
  end

  @impl true
  def describe(opts) do
    "user.#{opts[:user_field]} == this_record.#{opts[:record_field]}"
  end

  @impl true
  def strict_check(user, request, options) do
    user_field = options[:user_field]
    record_field = options[:record_field]

    case Ash.Filter.parse(request.resource, [{record_field, eq: Map.get(user, user_field)}]) do
      %{errors: []} = parsed ->
        if Ash.Filter.strict_subset_of?(parsed, request.filter) do
          {:ok, true}
        else
          {:ok, :unknown}
        end

      %{errors: errors} ->
        {:error, errors}
    end
  end

  @impl true
  def check(user, records, _request, options) do
    user_value = Map.fetch(user, options[:user_field])

    matches =
      Enum.filter(records, fn record ->
        user_value == Map.fetch(record, options[:record_field])
      end)

    {:ok, matches}
  end
end
