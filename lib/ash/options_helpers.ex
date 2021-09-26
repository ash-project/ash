defmodule Ash.OptionsHelpers do
  @moduledoc """
  Helpers for working with nimble options
  """

  @type schema :: NimbleOptions.schema()

  def merge_schemas(left, right, section \\ nil) do
    new_right =
      Enum.map(right, fn {key, value} ->
        {key, Keyword.put(value, :subsection, section)}
      end)

    Keyword.merge(left, new_right)
  end

  def validate(opts, schema) do
    NimbleOptions.validate(opts, sanitize_schema(schema))
  end

  def validate!(opts, schema) do
    NimbleOptions.validate!(opts, sanitize_schema(schema))
  end

  def docs(schema) do
    schema
    |> Enum.reject(fn {_key, opts} ->
      opts[:hide]
    end)
    |> sanitize_schema()
    |> Enum.map(fn {key, opts} ->
      if opts[:doc] do
        {key, Keyword.update!(opts, :doc, &String.replace(&1, "\n\n", "  \n"))}
      else
        {key, opts}
      end
    end)
    |> NimbleOptions.docs()
  end

  defp sanitize_schema(schema) do
    Enum.map(schema, fn {key, opts} ->
      new_opts =
        case opts[:type] do
          {:one_of, values} ->
            Keyword.put(opts, :type, {:in, values})

          {:ash_behaviour, behaviour, builtins} ->
            Keyword.put(opts, :type, {:custom, __MODULE__, :ash_behaviour, [behaviour, builtins]})

          :ash_resource ->
            Keyword.put(opts, :type, :atom)

          :ash_type ->
            # We don't want to add compile time dependencies on types
            # TODO: consider making this a legitimate validation
            Keyword.put(opts, :type, :any)

          _ ->
            opts
        end

      {key, Keyword.drop(new_opts, [:hide, :as, :snippet])}
    end)
  end

  def ash_behaviour({module, opts}, _behaviour, _builtins) when is_atom(module) do
    if Keyword.keyword?(opts) do
      # We can't check if it implements the behaviour here, unfortunately
      # As it may not be immediately available
      {:ok, {module, opts}}
    else
      {:error, "Expected opts to be a keyword, got: #{inspect(opts)}"}
    end
  end

  def ash_behaviour(module, behaviour, builtins) when is_atom(module) do
    ash_behaviour({module, []}, behaviour, builtins)
  end

  def ash_behaviour(other, _, _) do
    {:error, "Expected a module and opts, got: #{inspect(other)}"}
  end

  def map(value) when is_map(value), do: {:ok, value}
  def map(_), do: {:error, "must be a map"}

  def list_of_atoms(value) do
    if is_list(value) and Enum.all?(value, &is_atom/1) do
      {:ok, value}
    else
      {:error, "Expected a list of atoms"}
    end
  end

  def module_and_opts({module, opts}) when is_atom(module) do
    if Keyword.keyword?(opts) do
      {:ok, {module, opts}}
    else
      {:error, "Expected the second element to be a keyword list, got: #{inspect(opts)}"}
    end
  end

  def module_and_opts({other, _}) do
    {:error, "Expected the first element to be a module, got: #{inspect(other)}"}
  end

  def module_and_opts(module) do
    module_and_opts({module, []})
  end

  def default(value) when is_function(value, 0), do: {:ok, value}

  def default({module, function, args})
      when is_atom(module) and is_atom(function) and is_list(args),
      do: {:ok, {module, function, args}}

  def default(value), do: {:ok, value}

  def make_required!(options, field) do
    Keyword.update!(options, field, &Keyword.put(&1, :required, true))
  end

  def make_optional!(options, field) do
    Keyword.update!(options, field, &Keyword.delete(&1, :required))
  end

  def set_type!(options, field, type) do
    Keyword.update!(options, field, &Keyword.put(&1, :type, type))
  end

  def set_default!(options, field, value) do
    Keyword.update!(options, field, fn config ->
      config
      |> Keyword.put(:default, value)
      |> Keyword.delete(:required)
    end)
  end

  def append_doc!(options, field, to_append) do
    Keyword.update!(options, field, fn opt_config ->
      Keyword.update(opt_config, :doc, to_append, fn existing ->
        existing <> " - " <> to_append
      end)
    end)
  end
end
