# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test do
  @moduledoc """
  Testing helpers for Ash.
  """

  require ExUnit.Assertions

  @doc """
  Assert that the given changeset, query, or action input has a matching error.

  Use the optional second argument to assert that the errors (all together) are of a specific class.
  """
  @spec assert_has_error(
          Ash.Changeset.t()
          | Ash.Query.t()
          | Ash.ActionInput.t()
          | {:error, term}
          | {:ok, term}
          | :ok,
          error_class :: Ash.Error.class_module(),
          (Ash.Error.t() -> boolean)
        ) :: Ash.Error.t() | no_return
  def assert_has_error(changeset_query_or_input, error_class \\ nil, callback, opts \\ [])

  def assert_has_error({:error, %{splode: splode} = error}, error_class, callback, opts) do
    error = splode.to_class(error)

    if error_class do
      ExUnit.Assertions.assert(error.__struct__ == error_class,
        message:
          "Expected the value to have errors of class #{inspect(error_class)}, got: #{inspect(error.__struct__)}"
      )
    end

    match = Enum.find(error.errors, callback)

    ExUnit.Assertions.assert(match,
      message:
        opts[:message] ||
          """
          Expected at least one error to match the provided callback, but none did.

          Errors:

          #{inspect(error.errors, pretty: true)}
          """
    )

    match
  end

  def assert_has_error({:ok, _record}, error_class, _callback, _opts), do: no_errors(error_class)
  def assert_has_error(:ok, error_class, _callback, _opts), do: no_errors(error_class)

  def assert_has_error(changeset_query_or_input, error_class, callback, opts) do
    type =
      case changeset_query_or_input do
        %Ash.Changeset{} -> "changeset"
        %Ash.Query{} -> "query"
        %Ash.ActionInput{} -> "action input"
      end

    error = Ash.Error.to_error_class(changeset_query_or_input)

    if error_class do
      ExUnit.Assertions.assert(error.__struct__ == error_class,
        message:
          "Expected the #{type} to have errors of class #{inspect(error_class)}, got: #{inspect(error.__struct__)}"
      )
    end

    match = Enum.find(error.errors, callback)

    ExUnit.Assertions.assert(match,
      message:
        opts[:message] ||
          """
          Expected at least one error to match the provided callback, but none did.

          Errors:

          #{inspect(error.errors, pretty: true)}
          """
    )

    match
  end

  defp no_errors(error_class) do
    message =
      if error_class do
        "Expected the value to have errors of class #{inspect(error_class)}, but it had no errors"
      else
        "Expected the value to have errors matching the provided callback, but it had no errors"
      end

    ExUnit.Assertions.flunk(message)
  end

  @doc """
  Refute that the given changeset, query, or action input has a matching error.

  The `error_class` argument has been deprecated and should not be used.
  """
  @spec refute_has_error(
          Ash.Changeset.t()
          | Ash.Query.t()
          | Ash.ActionInput.t()
          | :ok
          | {:ok, term}
          | {:error, term},
          error_class :: Ash.Error.class_module(),
          (Ash.Error.t() -> boolean)
        ) :: Ash.Error.t() | no_return
  def refute_has_error(changeset_query_or_input, error_class \\ nil, callback, opts \\ [])

  # An :ok response doesn't have any errors!
  def refute_has_error(:ok, _error_class, _callback, _opts), do: :ok
  def refute_has_error({:ok, _record}, _error_class, _callback, _opts), do: :ok

  def refute_has_error({:error, error}, error_class, callback, opts) do
    if error_class != nil do
      IO.warn("`error_class` argument to `refute_has_error` is deprecated and will be ignored")
    end

    error = Ash.Error.to_error_class(error)
    match = Enum.find(error.errors, callback)

    ExUnit.Assertions.assert(!match,
      message:
        opts[:message] ||
          """
          Expected no errors to match the provided callback, but one did.

          Errors:

          #{inspect(match, pretty: true)}
          """
    )

    match
  end

  def refute_has_error(changeset_query_or_input, error_class, callback, opts) do
    if error_class != nil do
      IO.warn("`error_class` argument to `refute_has_error` is deprecated and will be ignored")
    end

    error = Ash.Error.to_error_class(changeset_query_or_input)
    match = Enum.find(error.errors, callback)

    ExUnit.Assertions.refute(match,
      message:
        opts[:message] ||
          """
          Expected no errors to match the provided callback, but one did.

          Matching Error:

          #{inspect(match, pretty: true)}

          Errors:

          #{inspect(error.errors, pretty: true)}
          """
    )

    match
  end

  @doc """
  Clears the `__metadata__` field and the underlying ecto `__meta__` field

  This allows for easier comparison of changeset/query results
  """
  def strip_metadata(structs) when is_list(structs), do: Enum.map(structs, &strip_metadata/1)

  def strip_metadata(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> strip_metadata()
    |> List.to_tuple()
  end

  def strip_metadata(%page_struct{results: results} = page)
      when page_struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    %{page | results: Enum.map(results, &strip_metadata/1)}
  end

  def strip_metadata(map) when is_map(map) do
    map
    |> Map.keys()
    |> Enum.reduce(map, fn key, map ->
      Map.update!(map, key, &strip_metadata_field(key, &1))
    end)
  end

  def strip_metadata(other), do: other

  defp strip_metadata_field(:__metadata__, _value), do: %{}
  defp strip_metadata_field(:__meta__, _value), do: %Ecto.Schema.Metadata{}
  defp strip_metadata_field(:__lateral_join_source__, _value), do: nil
  defp strip_metadata_field(:__order__, _value), do: nil
  defp strip_metadata_field(_key, value), do: strip_metadata(value)

  @doc """
  A macro for comparing Ash resources while ignoring metadata differences.

  ## Overview

  Ash resources contain metadata fields (`__metadata__` and `__meta__`) that track internal state like
  loaded relationships and action history. These fields can cause equality comparisons to fail even
  when the actual data is identical. This macro uses `strip_metadata/1` to remove these fields before
  comparison.

  ## Usage

  Use when comparing resources that have gone through different processing paths, such as comparing
  seeded data with data that has been processed through resource actions.

  ## Supported Operators

  * `==` and `===` - Equality comparison
  * `!=` and `!==` - Inequality comparison
  * `in` and `not in` - Membership testing

  ## Examples

      # Compare resources
      assert_stripped user1 == user2
      assert_stripped [user1, user2] === [user3, user4]
      assert_stripped user1 in [user2, user3, user4]
      assert_stripped user1 not in [user2, user3, user4]
  """
  defmacro assert_stripped({:==, _meta, [left, right]}) do
    quote do
      require ExUnit.Assertions
      ExUnit.Assertions.assert(strip_metadata(unquote(left)) == strip_metadata(unquote(right)))
    end
  end

  defmacro assert_stripped({:===, _meta, [left, right]}) do
    quote do
      require ExUnit.Assertions
      ExUnit.Assertions.assert(strip_metadata(unquote(left)) === strip_metadata(unquote(right)))
    end
  end

  defmacro assert_stripped({:!=, _meta, [left, right]}) do
    quote do
      require ExUnit.Assertions
      ExUnit.Assertions.assert(strip_metadata(unquote(left)) != strip_metadata(unquote(right)))
    end
  end

  defmacro assert_stripped({:!==, _meta, [left, right]}) do
    quote do
      require ExUnit.Assertions
      ExUnit.Assertions.assert(strip_metadata(unquote(left)) !== strip_metadata(unquote(right)))
    end
  end

  defmacro assert_stripped({:in, _meta, [left, right]}) do
    quote do
      require ExUnit.Assertions
      ExUnit.Assertions.assert(strip_metadata(unquote(left)) in strip_metadata(unquote(right)))
    end
  end

  defmacro assert_stripped({:not, _meta, [{:in, _meta2, [left, right]}]}) do
    quote do
      require ExUnit.Assertions

      ExUnit.Assertions.assert(
        strip_metadata(unquote(left)) not in strip_metadata(unquote(right))
      )
    end
  end

  defmacro assert_stripped(expression) do
    quote do
      raise ArgumentError,
        message: """
        assert_stripped received an unsupported operator or function.

        Expected one of the following:

        * assert_stripped left == right
        * assert_stripped left === right
        * assert_stripped left != right
        * assert_stripped left !== right
        * assert_stripped left in right
        * assert_stripped left not in right

        Got:

        assert_stripped #{Macro.to_string(unquote(Macro.escape(expression)))}
        """
    end
  end
end
