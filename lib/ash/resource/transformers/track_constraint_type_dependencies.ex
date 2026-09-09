# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.TrackConstraintTypeDependencies do
  @moduledoc """
  Registers compile dependencies for the types reachable from a field's constraints.

  `constraints` is in `no_depend_modules`, because the module references it can hold do not
  share one dependency requirement. `instance_of` is only read at runtime, while a nested
  `type` is initialized while the DSL is compiled, so its compile-time state would go stale
  if the resource stopped recompiling when it changed.

  Which of the two a module reference is, is known by the type, not by the DSL: only
  `Ash.Type.Struct` knows what `instance_of` means. So the dependency is registered here,
  from what `c:Ash.Type.referenced_types/1` reports, rather than declared in
  `Ash.Resource.Dsl` against constraint keys the DSL is otherwise unaware of.

  Opt in with:

      config :ash, :constraint_dependencies_from_referenced_types?, true

  A type that references other types without implementing `c:Ash.Type.referenced_types/1`
  keeps its compile dependencies while this is off.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after_compile?, do: false

  # Runs as a persister rather than a transformer: `:auto` types are replaced with real ones
  # by `ResolveAutoTypes`, which is itself a persister, and Spark silently ignores ordering
  # declarations that cross the two phases.
  def after?(Ash.Resource.Transformers.ResolveAutoTypes), do: true
  def after?(_other), do: false

  def transform(dsl_state) do
    # Without the opt-in, `constraints` still carries its own compile dependencies and there
    # is nothing to put back.
    with true <- Ash.Resource.Dsl.constraint_dependencies_from_referenced_types?(),
         %Macro.Env{} = env <- Transformer.get_persisted(dsl_state, :env) do
      Enum.each(typed_fields(dsl_state), &register(&1, env))
    end

    {:ok, dsl_state}
  end

  # The trace has to happen against the env of the module being compiled, which Spark
  # persists for us. Only the dependency is wanted here: `init/1` has already run for these
  # types, through the containing type's `init/2`, so calling it again would repeat that
  # work and invoke a callback that is not required to tolerate a second invocation.
  defp register({type, constraints}, env) do
    type
    |> Ash.Type.constraint_referenced_types(constraints)
    |> Enum.each(fn {referenced, _constraints} ->
      Macro.compile_apply(referenced, :__info__, [:module], env)
    end)
  end

  # Only fields whose `type` is an Ash type. A generic action carries `type: :action`
  # alongside the `constraints` of its `returns`, so entities are read by name rather than by
  # looking for a `type` key.
  defp typed_fields(dsl_state) do
    actions = Ash.Resource.Info.actions(dsl_state)
    calculations = Ash.Resource.Info.calculations(dsl_state)

    arguments = Enum.flat_map(actions ++ calculations, &Map.get(&1, :arguments, []))
    metadata = Enum.flat_map(actions, &Map.get(&1, :metadata, []))

    # Not `Ash.Resource.Info.interfaces/1`: it keeps only `Ash.Resource.Interface`, while
    # `define_calculation` builds an `Ash.Resource.CalculationInterface`, whose custom inputs
    # carry constraints just the same.
    custom_inputs =
      dsl_state
      |> Spark.Dsl.Extension.get_entities([:code_interface])
      |> Enum.flat_map(&Map.get(&1, :custom_inputs, []))

    returns =
      Enum.flat_map(actions, fn action ->
        typed(Map.get(action, :returns), Map.get(action, :constraints))
      end)

    fields =
      Ash.Resource.Info.attributes(dsl_state) ++
        Ash.Resource.Info.aggregates(dsl_state) ++
        calculations ++ arguments ++ metadata ++ custom_inputs

    returns ++ Enum.flat_map(fields, &typed(Map.get(&1, :type), Map.get(&1, :constraints)))
  end

  defp typed(nil, _constraints), do: []
  defp typed(:auto, _constraints), do: []
  defp typed(type, constraints), do: [{type, constraints || []}]
end
