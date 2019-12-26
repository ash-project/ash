defmodule Ash.Authorization.Check.AttributeEquals do
  use Ash.Authorization.Check

  def attribute_equals(field, value) do
    {__MODULE__, field: field, value: value}
  end

  @impl true
  def describe(opts) do
    "record.#{opts[:field]} == #{inspect(opts[:value])}"
  end

  @impl true
  def strict_check(_user, request, options) do
    field = options[:field]
    value = options[:value]

    case Ash.Filter.parse(request.resource, [{field, eq: value}]) do
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
