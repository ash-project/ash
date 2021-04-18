defmodule Ash.Resource.Validation.Present do
  @moduledoc false
  use Ash.Resource.Validation

  alias Ash.Error.Changes.{InvalidAttribute, InvalidChanges}

  @opt_schema [
    at_least: [
      type: :non_neg_integer,
      doc: "At least this many must be present. Defaults to the number of attributes provided"
    ],
    at_most: [
      type: :non_neg_integer,
      doc: "At most this many must be present. Defaults to the number of attributes provided"
    ],
    exactly: [
      type: :non_neg_integer,
      doc: "Exactly this many must be present"
    ],
    attributes: [
      type: {:custom, __MODULE__, :attributes, []},
      required: true,
      doc: "The attributes that the configured amount of must be present"
    ]
  ]

  @doc false
  def schema, do: @opt_schema

  @impl true
  def init(opts) do
    case Ash.OptionsHelpers.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def validate(changeset, opts) do
    {present, count} =
      Enum.reduce(opts[:attributes], {0, 0}, fn attribute, {present, count} ->
        if is_nil(Ash.Changeset.get_attribute(changeset, attribute)) do
          {present, count + 1}
        else
          {present + 1, count + 1}
        end
      end)

    opts =
      opts
      |> Keyword.put(:keys, Enum.join(opts[:attributes] || [], ","))
      |> Keyword.put(:fields, opts[:attributes])

    cond do
      opts[:exactly] && present != opts[:exactly] ->
        if opts[:exactly] == 0 do
          changes_error(opts, count, "must be absent")
        else
          if count == 1 do
            attribute_error(opts, count, "must be present")
          else
            attribute_error(
              opts,
              count,
              "exactly %{exactly} of %{keys} must be present"
            )
          end
        end

      opts[:at_least] && present < opts[:at_least] ->
        if count == 1 do
          attribute_error(opts, count, "must be present")
        else
          changes_error(opts, count, "at least %{at_least} of %{keys} must be present")
        end

      opts[:at_most] && present > opts[:at_most] ->
        if count == 1 do
          attribute_error(opts, count, "must not be present")
        else
          changes_error(opts, count, "at least %{at_most} of %{keys} must be present")
        end

      true ->
        :ok
    end
  end

  defp changes_error(opts, _count, message) do
    {:error,
     InvalidChanges.exception(
       fields: opts[:attributes],
       message: message,
       vars: opts
     )}
  end

  defp attribute_error(opts, _count, message) do
    {:error,
     opts[:attributes]
     |> List.wrap()
     |> Enum.map(fn attribute ->
       InvalidAttribute.exception(
         field: attribute,
         message: message,
         vars: opts
       )
     end)}
  end

  @doc false
  def attributes(attributes) do
    attributes = List.wrap(attributes)

    if Enum.all?(attributes, &is_atom/1) do
      {:ok, attributes}
    else
      {:error, "Expected all attributes provided to be atoms."}
    end
  end
end
