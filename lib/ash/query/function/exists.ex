# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Exists do
  @moduledoc """
  Determines if a given related entity exists.

  ## Path forms

  The `path` argument is a dotted chain of segments. Each segment may be:

    * a relationship — the usual case (`exists(comments, ...)`,
      `exists(posts.comments, ...)`).
    * an attribute whose type is `{:array, EmbeddedResource}`, where
      `EmbeddedResource` is declared with `use Ash.Resource, data_layer: :embedded`.
      Each element of the array is iterated and the predicate is evaluated
      against it. Mixed paths (`exists(invoices.options, ...)`) compose
      naturally — the relationship prefix is joined first, the embedded
      array attribute is unnested afterwards.

  ## `parent/1` semantics

  Each `exists/2` call pushes the *calling* scope onto the parent stack
  once, regardless of how many segments are in `path`. Inside the
  predicate, `parent/1` therefore always refers to the resource the
  `exists/2` call was made from, not to any intermediate scope along
  the path.

  > Note: `exists(a, exists(b, ...))` is auto-flattened by
  > `Ash.Query.Exists.new/3` into `exists(a.b, ...)`, so explicit
  > nesting does **not** create separate intermediate scopes for
  > `parent/1` to reach.

  ## Data layer support for embedded arrays

  The embedded-array form requires the data layer to declare the
  `{:exists, :embedded_array}` capability. AshPostgres supports it
  (lowering to `jsonb_array_elements`). Other SQL data layers and any
  in-memory layer with a generic record walker (e.g. `Ash.DataLayer.Ets`)
  work as well. If the data layer does not support this form, a clear
  error is raised at query build time.
  """

  defstruct [:path, :expr, :resource, at_path: [], related?: true, input?: false]

  def new(path, expr, at_path \\ [])

  def new([], expr, _) do
    raise "Cannot construct an exists query with an empty path, at #{inspect(%__MODULE__{path: [], expr: expr})}"
  end

  def new(path, %__MODULE__{at_path: [], expr: inner_expr, path: inner_path}, at_path) do
    %__MODULE__{at_path: at_path, expr: inner_expr, path: path ++ inner_path}
  end

  def new(path, expr, at_path) do
    %__MODULE__{path: path, expr: expr, at_path: at_path}
  end

  def can_return_nil?(_), do: false

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(
          %{path: path, expr: expr, at_path: at_path, related?: related?, resource: resource},
          opts
        ) do
      path_or_resource =
        if related? do
          Enum.join(path, ".")
        else
          inspect(resource)
        end

      if at_path && at_path != [] do
        concat([
          Enum.join(at_path, "."),
          ".",
          "exists(",
          path_or_resource,
          ", ",
          to_doc(expr, opts),
          ")"
        ])
      else
        concat(["exists(", path_or_resource, ", ", to_doc(expr, opts), ")"])
      end
    end
  end
end
