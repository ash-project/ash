defmodule Ash.Authorization.Check.BuiltInChecks do
  @moduledoc "The global authorization checks built into ash"

  def always() do
    {Ash.Authorization.Check.Static, [result: true]}
  end

  def never() do
    {Ash.Authorization.Check.Static, [result: false]}
  end

  def attribute_equals(field, value) do
    {Ash.Authorization.Check.AttributeEquals, field: field, value: value}
  end

  defmacro related_to_user_via(relationship) do
    quote do
      {Ash.Authorization.Check.RelatedToUserVia,
       relationship: List.wrap(unquote(relationship)), source: __MODULE__}
    end
  end

  def setting_attribute(name, opts) do
    opts =
      opts
      |> Keyword.take([:to])
      |> Keyword.put(:attribute_name, name)

    Ash.Authorization.Check.AttributeBuiltInChecks.setting(opts)
  end

  def user_attribute(field, value) do
    {Ash.Authorization.Check.UserAttribute, field: field, value: value}
  end

  def user_attribute_matches_record(user_field, record_field) do
    {Ash.Authorization.Check.UserAttributeMatchesRecord,
     user_field: user_field, record_field: record_field}
  end
end
