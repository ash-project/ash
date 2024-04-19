defmodule Ash.Resource.Change.SetAttribute do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset

  @opt_schema [
    attribute: [
      doc: "The attribute to change.",
      required: true,
      type: :atom
    ],
    value: [
      doc:
        "The value to set the attribute to; may be a fn/0 which will be called to produce the value.",
      required: true,
      type: {:custom, __MODULE__, :validate_value, []}
    ],
    set_when_nil?: [
      doc: "When false, decline setting the attribute if it is nil.",
      type: :boolean,
      default: true
    ],
    new?: [
      doc:
        "When true, sets the attribute to the value provided if the attribute is not already being changed.",
      type: :boolean,
      default: false
    ]
  ]

  def opt_schema, do: @opt_schema

  @impl true
  def init(opts) do
    case Spark.Options.validate(opts, opt_schema()) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  def validate_value(value) when is_function(value, 0), do: {:ok, value}

  def validate_value(value) when is_function(value),
    do: {:error, "only 0 argument functions are supported"}

  def validate_value(value), do: {:ok, value}

  @impl true
  def change(changeset, opts, _) do
    value =
      case opts[:value] do
        value when is_function(value) -> value.()
        value -> value
      end

    if opts[:new?] do
      if Ash.Changeset.changing_attribute?(changeset, opts[:attribute]) do
        changeset
      else
        Changeset.force_change_attribute(changeset, opts[:attribute], value)
      end
    else
      if opts[:set_when_nil?] or
           Changeset.get_attribute(changeset, opts[:attribute]) != nil do
        Changeset.force_change_attribute(changeset, opts[:attribute], value)
      else
        changeset
      end
    end
  end

  @impl true
  def atomic(_changeset, opts, _context) do
    value =
      case opts[:value] do
        value when is_function(value) -> value.()
        value -> value
      end

    {:atomic, %{opts[:attribute] => value}}
  end
end
