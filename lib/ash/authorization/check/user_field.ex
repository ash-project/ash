defmodule Ash.Authorization.Check.UserField do
  @moduledoc """
  This check allows access if a field on the resource directly matches a
  field on the user.
  """
  use Ash.Authorization.Check

  def init(opts) do
    with {:user, {:ok, user_field}} <- {:user, Keyword.fetch(opts, :user_field)},
         {:record, {:ok, record_field}} <- {:record, Keyword.fetch(opts, :record_field)} do
      {:ok, [record_field: record_field, user_field: user_field]}
    else
      {:user, :error} -> {:error, "Must supply `:user_field`"}
      {:record, :error} -> {:error, "Must supply `:record_field`"}
    end
  end

  def check(nil, _, _, _), do: false

  def check(user, data, _, opts) do
    user_value = Map.get(user, opts[:user_field])

    data
    |> Stream.filter(fn record ->
      Map.get(record, opts[:record_field]) == user_value
    end)
    |> Enum.map(& &1.id)
  end

  def describe(opts) do
    "the current user is the #{opts[:relationship]}"
  end

  def precheck(nil, _, _), do: {:precheck, false}

  def precheck(user, %{changeset: changeset}, opts) do
    value_will_equal_field? =
      changeset
      |> Ecto.Changeset.get_field(opts[:record_field])
      |> Kernel.==(Map.get(user, opts[:user_field]))

    {:precheck, value_will_equal_field?}
  end

  def precheck(user, context, opts) do
    user_value = Map.get(user, opts[:user_field])
    record_field = opts[:record_field]

    {:precheck, match?(%{params: %{filter: %{^record_field => ^user_value}}}, context)}
  end
end
