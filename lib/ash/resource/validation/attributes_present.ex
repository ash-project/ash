defmodule Ash.Resource.Validation.AttributesPresent do
  @moduledoc false
  use Ash.Resource.Validation

  alias Ash.Resource.Validation.Present
  import Ash.Expr

  def opt_schema, do: Present.opt_schema()

  @impl true
  def init(opts) do
    Present.init(opts)
  end

  @impl true
  def validate(changeset, opts, _context) do
    {present, count} =
      Enum.reduce(opts[:attributes], {0, 0}, fn attribute, {present, count} ->
        if Ash.Changeset.attribute_present?(changeset, attribute) do
          {present + 1, count + 1}
        else
          {present, count + 1}
        end
      end)

    Present.do_validate(changeset, opts, present, count)
  end

  @impl true
  def atomic(_changeset, opts, context) do
    values =
      Enum.map(opts[:attributes], fn attr ->
        expr(^atomic_ref(attr))
      end)

    Present.atomic_for_values(opts, context, values)
  end
end
