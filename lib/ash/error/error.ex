defmodule Ash.Error do
  @moduledoc """
  Tools and utilities used by Ash to manage and conform errors
  """

  alias Ash.Error.{Forbidden, Framework, Invalid, Stacktrace, Unknown}
  alias Ash.Error.Unknown.UnknownError

  @type error_class() :: :forbidden | :invalid | :framework | :unknown
  @type class_module() :: Forbidden | Framework | Invalid | Unknown

  @type t :: %{
          required(:__struct__) => module,
          required(:__exception__) => true,
          required(:class) => error_class(),
          required(:path) => [atom | integer],
          required(:changeset) => Ash.Changeset.t() | nil,
          required(:query) => Ash.Query.t() | nil,
          required(:error_context) => list(String.t()),
          required(:vars) => Keyword.t(),
          required(:stacktrace) => Ash.Error.Stacktrace.t() | nil,
          optional(atom) => any
        }

  @type class_error :: %{
          required(:__struct__) => class_module(),
          required(:__exception__) => true,
          required(:class) => error_class(),
          required(:path) => [atom | integer],
          required(:changeset) => Ash.Changeset.t() | nil,
          required(:query) => Ash.Query.t() | nil,
          required(:error_context) => list(String.t()),
          required(:vars) => Keyword.t(),
          required(:stacktrace) => Ash.Error.Stacktrace.t() | nil,
          optional(atom) => any
        }

  # We use these error classes also to choose a single error
  # to raise when multiple errors have occurred. We raise them
  # sorted by their error classes
  @error_classes [
    :forbidden,
    :invalid,
    :framework,
    :unknown
  ]

  @error_modules [
    forbidden: Forbidden,
    invalid: Invalid,
    framework: Framework,
    unknown: Unknown
  ]

  @error_class_indices @error_classes |> Enum.with_index() |> Enum.into(%{})

  @doc false
  def error_modules, do: Keyword.values(@error_modules)

  @doc false
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
        %{error | changeset: %{error.changeset | errors: set_path(error.changeset.errors, path)}}
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

  def ash_error?(value) do
    !!Ash.ErrorKind.impl_for(value)
  end

  @doc """
  Conforms a term into one of the built-in Ash [Error classes](handle-errors.html#error-classes).

  The provided term would usually be an Ash Error or a list of Ash Errors.

  If the term is:
  - a map/struct/Ash Error with a key `:class` having a value `:special`,
  - a list with a single map/struct/Ash Error element as above, or
  - an `Ash.Error.Invalid` containing such a list in its `:errors` field

  then the term is returned unchanged.

  Example:
  ```elixir

  iex(1)> Ash.Error.to_error_class("oops", changeset: Ash.Changeset.new(%Post{}), error_context: "some context")
    %Ash.Error.Unknown{
      changeset: #Ash.Changeset<
        errors: [
          %Ash.Error.Unknown.UnknownError{
            changeset: nil,
            class: :unknown,
            error: "oops",
            error_context: ["some context"],
            field: nil,
            path: [],
            query: nil,
            stacktrace: #Stacktrace<>,
            vars: []
          }
        ],
        ...
      >,
      class: :unknown,
      error_context: ["some context"],
      errors: [
        %Ash.Error.Unknown.UnknownError{
          changeset: nil,
          class: :unknown,
          error: "oops",
          error_context: ["some context"],
          field: nil,
          path: [],
          query: nil,
          stacktrace: #Stacktrace<>,
          vars: []
        }
      ],
      stacktrace: #Stacktrace<>,
      vars: []
    }

  ```

  Example of nested errors:
  ```elixir
    iex(1)> error1 = Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")
    iex(2)> error2 = Ash.Error.to_ash_error("whoops, again!!", nil, error_context: "some other context")
    iex(3)> Ash.Error.to_error_class([error1, error2], error_context: "some higher context")
    %Ash.Error.Unknown{
      changeset: nil,
      class: :unknown,
      error_context: ["some higher context"],
      errors: [
        %Ash.Error.Unknown.UnknownError{
          changeset: nil,
          class: :unknown,
          error: "whoops!",
          error_context: ["some higher context", "some context"],
          field: nil,
          path: [],
          query: nil,
          stacktrace: #Stacktrace<>,
          vars: []
        },
        %Ash.Error.Unknown.UnknownError{
          changeset: nil,
          class: :unknown,
          error: "whoops, again!!",
          error_context: ["some higher context", "some other context"],
          field: nil,
          path: [],
          query: nil,
          stacktrace: #Stacktrace<>,
          vars: []
        }
      ],
      path: [],
      query: nil,
      stacktrace: #Stacktrace<>,
      vars: []
    }

  ```

  Options:
  - `changeset`: a changeset related to the error
  - `query`: a query related to the error
  - `error_context`: a sting message providing extra context around the error
  """
  @spec to_error_class(any()) :: class_error()
  def to_error_class(values, opts \\ [])

  def to_error_class(%struct{errors: errors} = thing, opts)
      when struct in [Ash.Changeset, Ash.Query, Ash.ActionInput] do
    opt =
      case struct do
        Ash.Changeset -> :changeset
        Ash.Query -> :query
        Ash.ActionInput -> :input
      end

    to_error_class(errors, Keyword.put(opts, opt, thing))
  end

  def to_error_class(%{class: :special} = special, _opts) do
    special
  end

  def to_error_class([%{class: :special} = special], _opts) do
    special
  end

  def to_error_class(%Ash.Error.Invalid{errors: [%{class: :special} = special]}, _opts) do
    special
  end

  def to_error_class(values, opts) when is_list(values) do
    values =
      if Keyword.keyword?(opts) do
        [to_ash_error(values, opts[:stacktrace], Keyword.delete(opts, :error_context))]
      else
        Enum.map(
          values,
          &to_ash_error(&1, opts[:stacktrace], Keyword.delete(opts, :error_context))
        )
      end

    case values do
      [%{class: :special} = exception] ->
        exception

      values ->
        values =
          values
          |> flatten_preserving_keywords()
          |> Enum.uniq_by(&clear_stacktraces/1)
          |> Enum.map(fn value ->
            if ash_error?(value) do
              value
            else
              exception_opts =
                if opts[:stacktrace] do
                  [error: value, stacktrace: %Stacktrace{stacktrace: opts[:stacktrace]}]
                else
                  [error: values]
                end

              UnknownError.exception(exception_opts)
            end
          end)
          |> Enum.uniq()

        values
        |> accumulate_error_context(opts[:error_context])
        |> choose_error(opts[:changeset] || opts[:query] || opts[:input])
        |> add_error_context(opts[:error_context])
    end
  end

  def to_error_class(value, opts) do
    value = to_ash_error(value, opts[:stacktrace], Keyword.delete(opts, :error_context))

    if value.__struct__ in Keyword.values(@error_modules) do
      value
      |> add_changeset_or_query_or_input(
        [value],
        opts[:changeset] || opts[:query] || opts[:input]
      )
      |> Map.put(:error_context, [opts[:error_context] | value.error_context])
    else
      to_error_class([value], opts)
    end
  end

  @doc """
  Converts a combination of a module and json input into an Ash exception.

  This is necessary for certain data layers to be able to encode and decode Ash exceptions
  from structs.
  """
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
        |> Enum.map(&atomize_safely/1)
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

  defp update_if_present(handled, key, fun) do
    if Map.has_key?(handled, key) do
      Map.update!(handled, key, fun)
    else
      handled
    end
  end

  @doc false
  def atomize_safely(value) do
    String.to_existing_atom(value)
  rescue
    _ ->
      :unknown
  end

  @doc """
  Converts a term into an Ash Error.

  The term could be a simple string, the second element in an `{:error, error}` tuple, an Ash Error, or a list of any of these.
  In most cases the returned error is an Ash.Error.Unknown.UnknownError.

  A stacktrace is added to the error, and any existing stacktrace (i.e. when the term is an Ash Error) is preserved.

  `to_ash_error` converts string(s) into UnknownError(s):
  ```elixir
    iex(1)> Ash.Error.to_ash_error("whoops!", nil, error_context: "some context")
    %Ash.Error.Unknown.UnknownError{
      changeset: nil,
      class: :unknown,
      error: "whoops!",
      error_context: ["some context"],
      field: nil,
      path: [],
      query: nil,
      stacktrace: #Stacktrace<>,
      vars: []
    }

    iex(2)> Ash.Error.to_ash_error(["whoops!", "whoops, again!!"], nil, error_context: "some context")
    [
      %Ash.Error.Unknown.UnknownError{
        changeset: nil,
        class: :unknown,
        error: "whoops!",
        error_context: ["some context"],
        field: nil,
        path: [],
        query: nil,
        stacktrace: #Stacktrace<>,
        vars: []
      },
      %Ash.Error.Unknown.UnknownError{
        changeset: nil,
        class: :unknown,
        error: "whoops, again!!",
        error_context: ["some context"],
        field: nil,
        path: [],
        query: nil,
        stacktrace: #Stacktrace<>,
        vars: []
      }
    ]
  ```

  `to_ash_error` can preserve error-like data from a keyword-list and accumulate context if called against an Ash Error:
  ```elixir
    iex(1)> err = Ash.Error.to_ash_error([vars: [:some_var], message: "whoops!"], nil, error_context: " some context")
    %Ash.Error.Unknown.UnknownError{
      changeset: nil,
      class: :unknown,
      error: "whoops!",
      error_context: ["some context"],
      field: nil,
      path: [],
      query: nil,
      stacktrace: #Stacktrace<>,
      vars: [:some_var]
    }
    iex(2)> Ash.Error.to_ash_error(err, nil, error_context: "some higher context")
    %Ash.Error.Unknown.UnknownError{
      changeset: nil,
      class: :unknown,
      error: "whoops!",
      error_context: ["some higher context", "some context"],
      field: nil,
      path: [],
      query: nil,
      stacktrace: #Stacktrace<>,
      vars: [:some_var]
    }
  ```

  Options:
  - `error_context`: a sting message providing extra context around the error
  """
  def to_ash_error(list, stacktrace \\ nil, opts \\ [])

  def to_ash_error(list, stacktrace, opts) when is_list(list) do
    if Keyword.keyword?(list) do
      list
      |> Keyword.take([:error, :vars])
      |> Keyword.put_new(:error, list[:message])
      |> Keyword.put_new(:value, list)
      |> UnknownError.exception()
      |> add_stacktrace(stacktrace)
      |> add_error_context(opts[:error_context])
    else
      case list do
        [item] ->
          to_ash_error(item, stacktrace, opts)

        list ->
          Enum.map(list, &to_ash_error(&1, stacktrace, opts))
      end
    end
  end

  def to_ash_error(%Spark.Options.ValidationError{message: message}, stacktrace, opts) do
    to_ash_error(Ash.Error.Action.InvalidOptions.exception(message: message), stacktrace, opts)
  end

  def to_ash_error(%Ash.Changeset{errors: errors}, stacktrace, opts) do
    to_ash_error(errors, stacktrace, opts)
  end

  def to_ash_error(%Ash.Query{errors: errors}, stacktrace, opts) do
    to_ash_error(errors, stacktrace, opts)
  end

  def to_ash_error(%Ash.ActionInput{errors: errors}, stacktrace, opts) do
    to_ash_error(errors, stacktrace, opts)
  end

  def to_ash_error(error, stacktrace, opts) when is_binary(error) do
    [error: error]
    |> UnknownError.exception()
    |> Map.put(:stacktrace, nil)
    |> add_stacktrace(stacktrace)
    |> add_error_context(opts[:error_context])
  end

  def to_ash_error(other, stacktrace, opts) do
    cond do
      ash_error?(other) ->
        other
        |> add_stacktrace(stacktrace)
        |> add_error_context(opts[:error_context])

      is_exception(other) ->
        [error: Exception.format(:error, other)]
        |> UnknownError.exception()
        |> Map.put(:stacktrace, nil)
        |> add_stacktrace(stacktrace)
        |> add_error_context(opts[:error_context])

      true ->
        [error: "unknown error: #{inspect(other)}"]
        |> UnknownError.exception()
        |> Map.put(:stacktrace, nil)
        |> add_stacktrace(stacktrace)
        |> add_error_context(opts[:error_context])
    end
  end

  defp add_stacktrace(%{stacktrace: _} = error, stacktrace) do
    stacktrace =
      case stacktrace do
        %Stacktrace{stacktrace: nil} ->
          nil

        nil ->
          nil

        stacktrace ->
          %Stacktrace{stacktrace: stacktrace}
      end

    %{error | stacktrace: stacktrace || error.stacktrace || fake_stacktrace()}
  end

  defp add_stacktrace(e, _), do: e

  defp fake_stacktrace do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    %Stacktrace{stacktrace: Enum.drop(stacktrace, 3)}
  end

  defp add_error_context(error, error_context) when is_binary(error_context) do
    %{error | error_context: [error_context | error.error_context]}
  end

  defp add_error_context(error, _) do
    error
  end

  defp accumulate_error_context(%{errors: [_ | _] = errors} = error, error_context)
       when is_binary(error_context) do
    updated_errors = accumulate_error_context(errors, error_context)

    %{error | errors: updated_errors}
  end

  defp accumulate_error_context(errors, error_context)
       when is_list(errors) and is_binary(error_context) do
    errors
    |> Enum.map(fn err ->
      err
      |> add_error_context(error_context)
      |> accumulate_error_context(error_context)
    end)
  end

  defp accumulate_error_context(error, _) do
    error
  end

  @doc "A utility to flatten a list, but preserve keyword list elements"
  def flatten_preserving_keywords(list) do
    if Keyword.keyword?(list) do
      [list]
    else
      Enum.flat_map(list, fn item ->
        cond do
          Keyword.keyword?(item) ->
            [item]

          is_list(item) ->
            item

          true ->
            [item]
        end
      end)
    end
  end

  def clear_stacktraces(%{stacktrace: stacktrace} = error) when not is_nil(stacktrace) do
    clear_stacktraces(%{error | stacktrace: nil})
  end

  def clear_stacktraces(%{errors: errors} = exception) when is_list(errors) do
    %{exception | errors: Enum.map(errors, &clear_stacktraces/1)}
  end

  def clear_stacktraces(error), do: error

  def choose_error(errors, changeset_or_query_or_input \\ nil)

  def choose_error([], changeset_or_query_or_input) do
    error = Ash.Error.Unknown.exception([])

    add_changeset_or_query_or_input(error, [], changeset_or_query_or_input)
  end

  def choose_error(errors, changeset_or_query_or_input) do
    errors = Enum.map(errors, &to_ash_error/1)

    [error | other_errors] =
      Enum.sort_by(errors, fn error ->
        # the second element here sorts errors that are already parent errors
        {Map.get(@error_class_indices, error.class),
         @error_modules[error.class] != error.__struct__}
      end)

    parent_error_module = @error_modules[error.class]

    top_level_error =
      if parent_error_module == error.__struct__ do
        %{error | errors: (error.errors || []) ++ other_errors}
      else
        parent_error_module.exception(errors: errors)
      end

    add_changeset_or_query_or_input(top_level_error, errors, changeset_or_query_or_input)
  end

  defp add_changeset_or_query_or_input(error, errors, changeset_or_query_or_input) do
    changeset = error.changeset || error.query || changeset_or_query_or_input

    if changeset_or_query_or_input do
      changeset_or_query_or_input = %{
        changeset_or_query_or_input
        | action_failed?: true,
          errors: List.wrap(errors) ++ changeset.errors
      }

      case changeset_or_query_or_input do
        %Ash.Changeset{} = changeset ->
          %{error | changeset: %{changeset | errors: Enum.uniq(changeset.errors)}}

        %Ash.Query{} = query ->
          %{error | query: %{query | errors: Enum.uniq(query.errors)}}

        %Ash.ActionInput{} = input ->
          %{error | input: %{input | errors: Enum.uniq(input.errors)}}
      end
    else
      error
    end
  end

  def error_messages(errors, custom_message, stacktraces? \\ true) do
    errors = Enum.map(errors, &to_ash_error/1)

    generic_message =
      errors
      |> List.wrap()
      |> Enum.group_by(& &1.class)
      |> Enum.sort_by(fn {group, _} -> Map.get(@error_class_indices, group) end)
      |> Enum.map_join("\n\n", fn {class, class_errors} ->
        header = header(class) <> "\n\n"

        if stacktraces? do
          header <>
            Enum.map_join(class_errors, "\n", fn
              error when is_binary(error) ->
                "* #{error}"

              %{stacktrace: %Stacktrace{stacktrace: stacktrace}} = class_error ->
                breadcrumb(class_error.error_context) <>
                  "* #{Exception.message(class_error)}\n" <>
                  path(class_error) <>
                  Enum.map_join(stacktrace, "\n", fn stack_item ->
                    "  " <> Exception.format_stacktrace_entry(stack_item)
                  end)
            end)
        else
          header <>
            Enum.map_join(class_errors, "\n", fn
              class_error when is_binary(class_error) ->
                "* #{class_error}"

              class_error ->
                breadcrumb(class_error.error_context) <>
                  "* #{Exception.message(class_error)}" <> path(class_error)
            end)
        end
      end)

    if custom_message do
      custom =
        custom_message
        |> List.wrap()
        |> Enum.map_join("\n", &"* #{&1}")

      "\n\n" <> custom <> generic_message
    else
      generic_message
    end
  end

  def error_descriptions(errors) do
    errors
    |> Kernel.||([])
    |> Enum.group_by(& &1.class)
    |> Enum.sort_by(fn {group, _} -> Map.get(@error_class_indices, group) end)
    |> Enum.map_join("\n\n", fn {class, class_errors} ->
      header = header(class) <> "\n\n"

      header <> Enum.map_join(class_errors, "\n", &"* #{Exception.message(&1)}")
    end)
  end

  defp path(%{path: path}) when path not in [[], nil] do
    "    at " <> to_path(path) <> "\n"
  end

  defp path(_), do: ""

  defp to_path(path) do
    Enum.map_join(path, ", ", fn item ->
      if is_list(item) do
        "[#{to_path(item)}]"
      else
        if is_binary(item) || is_atom(item) || is_number(item) do
          item
        else
          inspect(item)
        end
      end
    end)
  end

  defp header(:forbidden), do: "Forbidden"
  defp header(:invalid), do: "Input Invalid"
  defp header(:framework), do: "Framework Error"
  defp header(:unknown), do: "Unknown Error"

  @doc false
  def breadcrumb(nil), do: ""
  def breadcrumb([]), do: ""

  def breadcrumb(error_context) do
    case Enum.filter(error_context, & &1) do
      [] ->
        ""

      bread_crumbs ->
        "Context: " <> Enum.join(bread_crumbs, " > ") <> "\n"
    end
  end
end
