# SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Splode do
  @moduledoc """
  Use this module to create your error aggregator and handler.

  For example:

  ```elixir
  defmodule MyApp.Errors do
    use Splode, error_classes: [
      invalid: MyApp.Errors.Invalid,
      unknown: MyApp.Errors.Unknown
    ],
    unknown_error: MyApp.Errors.Unknown.Unknown
  end
  ```

  ## Options

  - `:error_classes` - A keyword list mapping error class atoms to error class modules.
    At least one error class must be provided.

  - `:unknown_error` - The module to use when an error cannot be converted to a known type.
    This is required.

  - `:merge_with` - A list of other Splode modules whose errors should be recognized and
    flattened when combined. Optional.

  - `:filter_stacktraces` - A list of modules or module prefixes to filter from stacktraces.
    For each consecutive sequence of frames matching any filter, only the deepest (last) frame
    is kept. This is useful for hiding internal implementation details from error stacktraces.
    Accepts atoms (exact module match) or strings (prefix match). Optional.

    Elixir standard library frames (Enum, Stream, List, Map, etc.) are treated as part of an
    active matching sequence but are not kept as the "deepest" frame. This prevents stdlib
    frames from appearing in filtered stacktraces when they're sandwiched between internal
    module calls.

    ```elixir
    defmodule MyApp.Errors do
      use Splode,
        error_classes: [invalid: MyApp.Errors.Invalid],
        unknown_error: MyApp.Errors.Unknown,
        filter_stacktraces: [MyApp.Internal, "MyApp.Internal."]
    end
    ```
  """

  @doc """
  Returns true if the given value is a splode error.
  """
  @callback splode_error?(term) :: boolean()

  @doc """
  Sets the path on the error or errors
  """
  @callback set_path(Splode.Error.t() | [Splode.Error.t()], term | list(term)) ::
              Splode.Error.t() | [Splode.Error.t()]

  @doc """
  Combine errors into an error class
  """
  @callback to_class(any()) :: Splode.Error.t()

  @doc """
  Turns any value into a splode error
  """
  @callback to_error(any()) :: Splode.Error.t()
  @doc """
  Converts a combination of a module and json input into an Splode exception.

  This allows for errors to be serialized and deserialized
  """
  @callback from_json(module, map) :: Splode.Error.t()

  @elixir_stdlib [
    Enum,
    Stream,
    Enumerable,
    Enumerable.List,
    Enumerable.Map,
    Enumerable.Function,
    Enumerable.Stream,
    List,
    Map,
    Keyword,
    Task,
    Agent,
    GenServer
  ]

  @doc false
  def filter_stacktrace(stacktrace, []), do: stacktrace

  def filter_stacktrace(stacktrace, filters) when is_list(stacktrace) do
    do_filter_stacktrace(stacktrace, filters, nil, [])
  end

  def filter_stacktrace(stacktrace, _filters), do: stacktrace

  defp do_filter_stacktrace([], _filters, nil, acc), do: Enum.reverse(acc)
  defp do_filter_stacktrace([], _filters, deepest, acc), do: Enum.reverse([deepest | acc])

  defp do_filter_stacktrace([{mod, _, _, _} = frame | rest], filters, current_deepest, acc) do
    cond do
      matches_filter?(mod, filters) ->
        do_filter_stacktrace(rest, filters, frame, acc)

      mod in @elixir_stdlib and current_deepest != nil ->
        do_filter_stacktrace(rest, filters, current_deepest, acc)

      true ->
        case current_deepest do
          nil -> do_filter_stacktrace(rest, filters, nil, [frame | acc])
          deepest -> do_filter_stacktrace(rest, filters, nil, [frame, deepest | acc])
        end
    end
  end

  defp do_filter_stacktrace([frame | rest], filters, current_deepest, acc) do
    case current_deepest do
      nil -> do_filter_stacktrace(rest, filters, nil, [frame | acc])
      deepest -> do_filter_stacktrace(rest, filters, nil, [frame, deepest | acc])
    end
  end

  defp matches_filter?(mod, filters) do
    Enum.any?(filters, fn
      matcher when is_atom(matcher) -> mod == matcher
      prefix when is_binary(prefix) -> String.starts_with?(inspect(mod), prefix)
    end)
  end

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], generated: true, location: :keep do
      @behaviour Splode
      @error_classes Keyword.put_new(
                       List.wrap(opts[:error_classes]),
                       :unknown,
                       Splode.Error.Unknown
                     )

      @unknown_error opts[:unknown_error] ||
                       raise(
                         ArgumentError,
                         "must supply the `unknown_error` option, pointing at a splode error to use in situations where we cannot convert an error."
                       )

      @merge_with List.wrap(opts[:merge_with])
      @filter_stacktraces List.wrap(opts[:filter_stacktraces])

      @doc false
      def __stacktrace_filters__, do: @filter_stacktraces

      if Enum.empty?(opts[:error_classes]) do
        raise ArgumentError,
              "must supply at least one error class to `use Splode`, via `use Splode, error_classes: [class: ModuleForClass]`"
      end

      @type error_class() ::
              unquote(@error_classes |> Keyword.keys() |> Enum.reduce(&{:|, [], [&1, &2]}))

      @type class_module() ::
              unquote(@error_classes |> Keyword.values() |> Enum.reduce(&{:|, [], [&1, &2]}))

      @type t :: %{
              required(:__struct__) => module(),
              required(:__exception__) => true,
              required(:class) => error_class(),
              required(:bread_crumbs) => list(String.t()),
              required(:vars) => Keyword.t(),
              required(:stacktrace) => Splode.Stacktrace.t() | nil,
              required(:context) => map(),
              optional(atom) => any
            }

      @type class :: %{
              required(:__struct__) => class_module(),
              required(:__exception__) => true,
              required(:errors) => list(t()),
              required(:class) => error_class(),
              required(:bread_crumbs) => list(String.t()),
              required(:vars) => Keyword.t(),
              required(:stacktrace) => Splode.Stacktrace.t() | nil,
              required(:context) => map(),
              optional(atom) => any
            }

      @class_modules Keyword.values(@error_classes) |> Enum.reject(&is_nil/1)

      @error_class_indices @error_classes |> Keyword.keys() |> Enum.with_index() |> Enum.into(%{})

      @doc """
      Raises an error if the result is an error, otherwise returns the result

      Alternatively, you can use the `defsplode` macro, which does this automatically.

      ### Options

      - `:error_opts` - Options to pass to `to_error/2` when converting the returned error
      - `:unknown_error_opts` - Options to pass to the unknown error if the function returns only `:error`.
        not necessary if your function always returns `{:error, error}`.

      ### Examples

        def function(arg) do
          case do_something(arg) do
            :success -> :ok
            {:success, result} -> {:ok, result}
            {:error, error} -> {:error, error}
          end
        end

        def function!(arg) do
          YourErrors.unwrap!(function(arg))
        end
      """
      def unwrap!(result, opts \\ nil)
      def unwrap!({:ok, result}, _opts), do: result
      def unwrap!(:ok, _), do: :ok

      def unwrap!({:error, error}, opts), do: raise(to_error(error, opts[:error_opts] || []))

      def unwrap!(:error, opts),
        do: raise(@error_classes[:unknown].exception(opts[:unknown_error_opts] || []))

      def unwrap!(other, opts),
        do:
          raise(
            ArgumentError,
            "Invalid value provided to `splode!/2`:\n\n#{inspect(other)}"
          )

      @impl true
      def set_path(errors, path) when is_list(errors) do
        Enum.map(errors, &set_path(&1, path))
      end

      def set_path(error, path) when is_map(error) do
        path = List.wrap(path)

        error =
          if Map.has_key?(error, :path) && is_list(error.path) do
            %{error | path: path ++ error.path}
          else
            error
          end

        error =
          if Map.has_key?(error, :changeset) && error.changeset do
            %{
              error
              | changeset: %{error.changeset | errors: set_path(error.changeset.errors, path)}
            }
          else
            error
          end

        if Map.has_key?(error, :errors) && is_list(error.errors) do
          %{error | errors: Enum.map(error.errors, &set_path(&1, path))}
        else
          error
        end
      end

      def set_path(error, _), do: error

      @impl true
      def splode_error?(%struct{}) do
        struct.splode_error?()
      rescue
        _ ->
          false
      end

      def splode_error?(_), do: false

      def splode_error?(%struct{splode: splode}, splode) do
        struct.splode_error?()
      rescue
        _ ->
          false
      end

      def splode_error?(%struct{splode: nil}, _splode) do
        struct.splode_error?()
      rescue
        _ ->
          false
      end

      def splode_error?(_, _), do: false

      @impl true
      def to_class(value, opts \\ [])

      def to_class(%struct{errors: [error]} = class, opts)
          when struct in @class_modules do
        if error.class == :special do
          error
        else
          class
          |> accumulate_bread_crumbs(opts[:bread_crumbs])
        end
      end

      def to_class(value, opts) when not is_list(value) do
        if splode_error?(value) && value.class == :special do
          Map.put(value, :splode, __MODULE__)
        else
          to_class([value], opts)
        end
      end

      def to_class(values, opts) when is_list(values) do
        errors =
          if Keyword.keyword?(values) && values != [] do
            [to_error(values, Keyword.delete(opts, :bread_crumbs))]
          else
            values
            |> flatten_preserving_keywords()
            |> Enum.map(fn error ->
              if Enum.any?([__MODULE__ | @merge_with], &splode_error?(error, &1)) do
                filter_error_stacktraces(error)
              else
                to_error(error, Keyword.delete(opts, :bread_crumbs))
              end
            end)
          end

        if Enum.count_until(errors, 2) == 1 &&
             (Enum.at(errors, 0).class == :special || Enum.at(errors, 0).__struct__.error_class?()) do
          List.first(errors)
          |> accumulate_bread_crumbs(opts[:bread_crumbs])
        else
          errors
          |> flatten_errors()
          |> Enum.uniq_by(&clear_stacktraces/1)
          |> Enum.map(fn value ->
            if Enum.any?([__MODULE__ | @merge_with], &splode_error?(value, &1)) do
              Map.put(value, :splode, value.splode || __MODULE__)
            else
              exception_opts =
                if opts[:stacktrace] do
                  filtered = Splode.filter_stacktrace(opts[:stacktrace], @filter_stacktraces)

                  [
                    error: value,
                    stacktrace: %Splode.Stacktrace{stacktrace: filtered},
                    splode: __MODULE__
                  ]
                else
                  [error: value, splode: __MODULE__]
                end

              @unknown_error.exception(exception_opts)
            end
          end)
          |> choose_error()
          |> accumulate_bread_crumbs(opts[:bread_crumbs])
          |> Map.put(:splode, __MODULE__)
        end
      end

      defp choose_error([]) do
        @error_classes[:unknown].exception(splode: __MODULE__)
      end

      defp choose_error(errors) do
        [error | other_errors] =
          Enum.sort_by(errors, fn error ->
            # the second element here sorts errors that are already parent errors
            {Map.get(@error_class_indices, error.class) ||
               Map.get(@error_class_indices, :unknown),
             @error_classes[error.class] != error.__struct__}
          end)

        parent_error_module =
          @error_classes[error.class] || Keyword.get(@error_classes, :unknown) ||
            Splode.Error.Unknown

        if parent_error_module == error.__struct__ do
          %{error | errors: (error.errors || []) ++ other_errors}
        else
          parent_error_module.exception(errors: errors, splode: __MODULE__)
        end
      end

      @impl true
      def to_error(value, opts \\ [])

      def to_error(list, opts) when is_list(list) do
        if Keyword.keyword?(list) do
          list
          |> Keyword.take([:error, :vars])
          |> Keyword.put_new(:error, list[:message])
          |> Keyword.put_new(:value, list)
          |> Keyword.put(:splode, __MODULE__)
          |> @unknown_error.exception()
          |> add_stacktrace(opts[:stacktrace])
          |> accumulate_bread_crumbs(opts[:bread_crumbs])
        else
          case list do
            [item] ->
              to_error(item, opts)

            list ->
              to_class(list, opts)
          end
        end
      end

      def to_error(error, opts) when is_binary(error) do
        [error: error, splode: __MODULE__]
        |> @unknown_error.exception()
        |> Map.put(:stacktrace, nil)
        |> add_stacktrace(opts[:stacktrace])
        |> accumulate_bread_crumbs(opts[:bread_crumbs])
      end

      def to_error(other, opts) do
        cond do
          Enum.any?([__MODULE__ | @merge_with], &splode_error?(other, &1)) ->
            other
            |> Map.put(:splode, other.splode || __MODULE__)
            |> add_stacktrace(opts[:stacktrace])
            |> accumulate_bread_crumbs(opts[:bread_crumbs])

          is_exception(other) ->
            [error: Exception.format(:error, other), splode: __MODULE__]
            |> @unknown_error.exception()
            |> add_stacktrace(opts[:stacktrace])
            |> accumulate_bread_crumbs(opts[:bread_crumbs])

          true ->
            [error: "unknown error: #{inspect(other)}", splode: __MODULE__]
            |> @unknown_error.exception()
            |> Map.put(:stacktrace, nil)
            |> add_stacktrace(opts[:stacktrace])
            |> accumulate_bread_crumbs(opts[:bread_crumbs])
        end
      end

      defp flatten_errors(errors) do
        errors
        |> Enum.flat_map(&List.wrap/1)
        |> Enum.flat_map(fn error ->
          if Enum.any?([__MODULE__ | @merge_with], &splode_error?(error, &1)) do
            if error.__struct__.error_class?() do
              flatten_errors(error.errors)
            else
              [error]
            end
          else
            [error]
          end
        end)
      end

      defp flatten_preserving_keywords(list) do
        if Keyword.keyword?(list) do
          [list]
        else
          Enum.flat_map(list, fn item ->
            cond do
              Keyword.keyword?(item) ->
                [item]

              is_list(item) ->
                flatten_preserving_keywords(item)

              true ->
                [item]
            end
          end)
        end
      end

      defp filter_error_stacktraces(%{stacktrace: %Splode.Stacktrace{stacktrace: trace}} = error)
           when is_list(trace) do
        error = %{
          error
          | stacktrace: %Splode.Stacktrace{
              stacktrace: Splode.filter_stacktrace(trace, @filter_stacktraces)
            }
        }

        if Map.has_key?(error, :errors) and is_list(error.errors) do
          %{error | errors: Enum.map(error.errors, &filter_error_stacktraces/1)}
        else
          error
        end
      end

      defp filter_error_stacktraces(%{errors: errors} = error) when is_list(errors) do
        %{error | errors: Enum.map(errors, &filter_error_stacktraces/1)}
      end

      defp filter_error_stacktraces(error), do: error

      defp add_stacktrace(%{stacktrace: _} = error, stacktrace) do
        stacktrace =
          case stacktrace do
            %Splode.Stacktrace{stacktrace: nil} ->
              nil

            nil ->
              nil

            %Splode.Stacktrace{stacktrace: trace} ->
              %Splode.Stacktrace{stacktrace: Splode.filter_stacktrace(trace, @filter_stacktraces)}

            stacktrace ->
              %Splode.Stacktrace{
                stacktrace: Splode.filter_stacktrace(stacktrace, @filter_stacktraces)
              }
          end

        existing_stacktrace =
          case error.stacktrace do
            %Splode.Stacktrace{stacktrace: trace} when is_list(trace) ->
              %Splode.Stacktrace{stacktrace: Splode.filter_stacktrace(trace, @filter_stacktraces)}

            other ->
              other
          end

        %{error | stacktrace: stacktrace || existing_stacktrace || fake_stacktrace()}
      end

      defp add_stacktrace(e, _), do: e

      defp fake_stacktrace do
        {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
        filtered = Splode.filter_stacktrace(Enum.drop(stacktrace, 3), @filter_stacktraces)
        %Splode.Stacktrace{stacktrace: filtered}
      end

      defp accumulate_bread_crumbs(error, bread_crumbs) when is_list(bread_crumbs) do
        bread_crumbs
        |> Enum.reverse()
        |> Enum.reduce(error, &accumulate_bread_crumbs(&2, &1))
      end

      defp accumulate_bread_crumbs(%module{errors: errors} = error, bread_crumbs)
           when is_binary(bread_crumbs) and module in @class_modules do
        updated_errors = accumulate_bread_crumbs(errors, bread_crumbs)

        add_bread_crumbs(%{error | errors: updated_errors}, bread_crumbs)
      end

      defp accumulate_bread_crumbs(errors, bread_crumbs)
           when is_list(errors) and is_binary(bread_crumbs) do
        Enum.map(errors, &add_bread_crumbs(&1, bread_crumbs))
      end

      defp accumulate_bread_crumbs(error, bread_crumbs) do
        add_bread_crumbs(error, bread_crumbs)
      end

      defp add_bread_crumbs(error, bread_crumbs) when is_list(bread_crumbs) do
        bread_crumbs
        |> Enum.reverse()
        |> Enum.reduce(error, &add_bread_crumbs(&2, &1))
      end

      defp add_bread_crumbs(error, bread_crumb) when is_binary(bread_crumb) do
        %{error | bread_crumbs: [bread_crumb | error.bread_crumbs]}
      end

      defp add_bread_crumbs(error, _) do
        error
      end

      @impl true
      def from_json(module, json) do
        {handled, unhandled} = process_known_json_keys(json)

        unhandled =
          Map.update(unhandled, "vars", [], fn vars ->
            Map.to_list(vars)
          end)

        json = Map.merge(unhandled, handled)

        module.from_json(json)
      end

      defp process_known_json_keys(json) do
        {handled, unhandled} = Map.split(json, ~w(field fields message path))

        handled =
          handled
          |> update_if_present("field", &String.to_existing_atom/1)
          |> update_if_present("fields", fn fields ->
            fields
            |> List.wrap()
            |> Enum.map(&Splode.Error.atomize_safely/1)
          end)
          |> update_if_present("path", fn item ->
            item
            |> List.wrap()
            |> Enum.map(fn
              item when is_integer(item) ->
                item

              item when is_binary(item) ->
                case Integer.parse(item) do
                  {integer, ""} -> integer
                  _ -> item
                end
            end)
          end)

        {handled, unhandled}
      end

      defp clear_stacktraces(%{stacktrace: stacktrace} = error) when not is_nil(stacktrace) do
        clear_stacktraces(%{error | stacktrace: nil})
      end

      defp clear_stacktraces(%{errors: errors} = exception) when is_list(errors) do
        %{exception | errors: Enum.map(errors, &clear_stacktraces/1)}
      end

      defp clear_stacktraces(error), do: error

      defp update_if_present(handled, key, fun) do
        if Map.has_key?(handled, key) do
          Map.update!(handled, key, fun)
        else
          handled
        end
      end

      defoverridable set_path: 2
    end
  end
end
