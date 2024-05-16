defmodule Ash.Policy.Check.ChangingAttributes do
  @moduledoc "This check is true when attribute changes correspond to the provided options."
  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    message =
      Enum.map_join(opts[:changing], " and ", fn
        {key, further} ->
          field_message =
            Enum.map_join(further, ", ", fn
              {:to, to_value} ->
                "to #{inspect(to_value)}"

              {:from, from_value} ->
                "from #{inspect(from_value)}"
            end)

          "#{key} #{field_message}"

        key ->
          "#{key}"
      end)

    "changing #{message}"
  end

  @impl true
  def filter(_actor, %{changeset: %Ash.Changeset{} = changeset}, options) do
    Enum.reduce_while(options[:changing], true, fn {attribute, opts}, expr ->
      if Keyword.has_key?(opts, :from) && changeset.action_type == :create do
        if opts[:from] == nil do
          {:halt, false}
        else
          {:cont, expr}
        end
      else
        if Ash.Changeset.changing_attribute?(changeset, attribute) do
          case {Keyword.fetch(opts, :from), Keyword.fetch(opts, :to)} do
            {:error, :error} ->
              {:cont, expr}

            {{:ok, from}, {:ok, to}} ->
              if expr == true do
                {:cont, expr(^ref(attribute) == ^from and ^atomic_ref(attribute) == ^to)}
              else
                {:cont,
                 expr(^expr and ^ref(attribute) == ^from and ^atomic_ref(attribute) == ^to)}
              end

            {{:ok, from}, :error} ->
              if expr == true do
                {:cont, expr(^ref(attribute) == ^from)}
              else
                {:cont, expr(^expr and ^ref(attribute) == ^from)}
              end

            {:error, {:ok, to}} ->
              if expr == true do
                {:cont, expr(^atomic_ref(attribute) == ^to)}
              else
                {:cont, expr(^expr and ^atomic_ref(attribute) == ^to)}
              end
          end
        else
          {:halt, false}
        end
      end
    end)
  end

  def filter(_, _, _), do: false
end
