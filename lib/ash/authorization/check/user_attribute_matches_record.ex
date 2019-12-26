defmodule Ash.Authorization.Check.UserAttributeMatchesRecord do
  use Ash.Authorization.Check

  def user_attribute_matches_record(user_field, record_field) do
    {__MODULE__, user_field: user_field, record_field: record_field}
  end

  @impl true
  def describe(opts) do
    "user.#{opts[:user_field]} == record.#{opts[:record_field]}"
  end

  @impl true
  def strict_check(user, request, options) do
    user_field = options[:user_field]
    record_field = options[:record_field]

    case Ash.Filter.parse(request.resource, [{record_field, eq: Map.get(user, user_field)}]) do
      %{errors: []} = parsed ->
        cond do
          Ash.Filter.contains?(parsed, request.filter) ->
            [decision: true]

          request.strict_access? ->
            [decision: false]

          true ->
            []
        end

      %{errors: errors} ->
        [error: errors]
    end
  end
end
