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

  def related_to_user_via(relationship) do
    {Ash.Authorization.Check.RelatedToUserVia, relationship: List.wrap(relationship)}
  end

  def setting_relationship(relationship) do
    {Ash.Authorization.Check.SettingRelationship, relationship_name: relationship}
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

  def relating_to_user(relationship_name, opts) do
    {Ash.Authorization.Check.RelatingToUser,
     Keyword.put(opts, :relationship_name, relationship_name)}
  end

  def relationship_set(relationship_name) do
    {Ash.Authorization.Check.RelationshipSet, [relationship_name: relationship_name]}
  end
end
