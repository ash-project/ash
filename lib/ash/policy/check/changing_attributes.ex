defmodule Ash.Policy.Check.ChangingAttributes do
  @moduledoc false
  use Ash.Policy.SimpleCheck

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
  def match?(_actor, %{changeset: %Ash.Changeset{} = changeset}, opts) do
    Enum.all?(opts[:changing], fn
      {attribute, opts} ->
        if Keyword.has_key?(opts, :from) && changeset.action_type == :create do
          false
        else
          case Ash.Changeset.fetch_change(changeset, attribute) do
            {:ok, new_value} ->
              opts == [] ||
                Enum.all?(opts, fn
                  {:to, value} ->
                    new_value == value

                  {:from, value} ->
                    Map.get(changeset.data, attribute) == value
                end)

            _ ->
              false
          end
        end

      attribute ->
        match?({:ok, _}, Ash.Changeset.fetch_change(changeset, attribute))
    end)
  end

  def match?(_, _, _), do: false
end
