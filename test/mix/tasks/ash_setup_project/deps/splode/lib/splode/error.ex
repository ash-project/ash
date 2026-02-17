# SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Splode.Error do
  @moduledoc """
  Use this module to create an aggregatable error.

  For example:

  ```elixir
  defmodule MyApp.Errors.InvalidArgument do
    use Splode.Error, fields: [:name, :message], class: :invalid

    def message(%{name: name, message: message}) do
      "Invalid argument \#{name}: \#{message}"
    end
  end
  ```
  """
  @callback splode_error?() :: boolean()
  @callback from_json(map) :: struct()
  @callback error_class?() :: boolean()
  @type t :: Exception.t()

  @doc false
  def atomize_safely(value) do
    String.to_existing_atom(value)
  rescue
    _ ->
      :unknown
  end

  defmacro __using__(opts) do
    quote generated: true, bind_quoted: [opts: opts, mod: __MODULE__] do
      @behaviour Splode.Error
      @error_class !!opts[:error_class?]

      if !opts[:class] do
        raise "Must provide an error class for a splode error, i.e `use Splode.Error, class: :invalid`"
      end

      defexception List.wrap(opts[:fields]) ++
                     [
                       splode: nil,
                       bread_crumbs: [],
                       vars: [],
                       path: [],
                       stacktrace: nil,
                       class: opts[:class]
                     ]

      @before_compile mod

      @impl Splode.Error
      def splode_error?, do: true

      @impl Splode.Error
      def error_class?, do: @error_class

      field_names =
        Enum.map(List.wrap(opts[:fields]), fn
          {k, _v} ->
            k

          k ->
            k
        end)

      @impl Exception
      @doc """
      Create an `#{__MODULE__}` without raising it.


      ## Keys

      #{Enum.map_join(field_names, "\n", &"- #{inspect(&1)}")}
      """
      @spec exception(opts :: Keyword.t()) :: %__MODULE__{}
      def exception(opts \\ []) do
        if @error_class && match?([%error{class: :special} = special], opts[:errors]) do
          special_error = Enum.at(opts[:errors], 0)

          if special_error.__struct__.splode_error?() do
            special_error
          else
            opts = Splode.Error.maybe_add_stacktrace(opts)
            super(opts) |> Map.update(:vars, [], &Splode.Error.clean_vars/1)
          end
        else
          opts = Splode.Error.maybe_add_stacktrace(opts)
          super(opts) |> Map.update(:vars, [], &Splode.Error.clean_vars/1)
        end
      end

      @impl Splode.Error
      def from_json(json) do
        keyword =
          json
          |> Map.to_list()
          |> Enum.map(fn {key, value} -> {Splode.Error.atomize_safely(key), value} end)

        exception(keyword)
      end

      defoverridable exception: 1, from_json: 1
    end
  end

  defmacro __before_compile__(env) do
    if Module.defines?(env.module, {:message, 1}, :def) &&
         !Module.get_attribute(env.module, :error_class) do
      quote generated: true do
        defoverridable message: 1

        @impl true
        def message(%{vars: vars} = exception) do
          string = super(exception)

          string =
            case Splode.ErrorClass.bread_crumb(exception.bread_crumbs) do
              "" ->
                string

              context ->
                context <> "\n" <> string
            end

          Enum.reduce(List.wrap(vars), string, fn {key, value}, acc ->
            if String.contains?(acc, "%{#{key}}") do
              String.replace(acc, "%{#{key}}", inspect(value))
            else
              acc
            end
          end)
        end
      end
    end
  end

  @doc false
  def clean_vars(vars) when is_map(vars) do
    clean_vars(Map.to_list(vars))
  end

  def clean_vars(vars) do
    vars |> Kernel.||([]) |> Keyword.drop([:field, :message, :path])
  end

  @doc false
  def maybe_add_stacktrace(opts) do
    if is_nil(opts[:stacktrace]) do
      {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
      stacktrace = Enum.drop(stacktrace, 1)
      stacktrace = filter_stacktrace_from_opts(stacktrace, opts)

      Keyword.put(opts, :stacktrace, %Splode.Stacktrace{stacktrace: stacktrace})
    else
      if is_list(opts[:stacktrace]) do
        stacktrace = filter_stacktrace_from_opts(opts[:stacktrace], opts)
        Keyword.put(opts, :stacktrace, %Splode.Stacktrace{stacktrace: stacktrace})
      else
        opts
      end
    end
  end

  defp filter_stacktrace_from_opts(stacktrace, opts) do
    splode = opts[:splode]

    if splode && function_exported?(splode, :__stacktrace_filters__, 0) do
      Splode.filter_stacktrace(stacktrace, splode.__stacktrace_filters__())
    else
      stacktrace
    end
  end
end
