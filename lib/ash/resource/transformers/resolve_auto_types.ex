defmodule Ash.Resource.Transformers.ResolveAutoTypes do
  @moduledoc """
  Resolves `:auto` types on calculations by analyzing their expressions.

  For expression calculations like `calculate :name, :auto, expr(title)`, this persister
  determines the type from the expression and updates the calculation.

  For cross-resource references, coordinates with `Ash.TypeResolver` to handle
  dependencies between resources being compiled in parallel.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after?(_), do: true

  def transform(dsl_state) do
    module = Transformer.get_persisted(dsl_state, :module)
    calculations = Ash.Resource.Info.calculations(dsl_state)
    auto_calcs = Enum.filter(calculations, &(&1.type == :auto))

    if auto_calcs == [] do
      {:ok, dsl_state}
    else
      # Validate all :auto calcs are expression calculations
      Enum.each(auto_calcs, &validate_expression_calc!(&1, module))

      # Try to resolve each :auto calc locally first (no TypeResolver needed)
      {resolved, unresolved} = resolve_locally(auto_calcs, module, dsl_state)

      dsl_state = apply_resolutions(dsl_state, resolved)

      if unresolved == [] do
        {:ok, dsl_state}
      else
        # Need cross-resource coordination
        resolve_with_type_resolver(dsl_state, module, unresolved, calculations)
      end
    end
  end

  defp resolve_locally(auto_calcs, module, dsl_state) do
    Enum.split_with(auto_calcs, fn calc ->
      expr = get_expression!(calc)

      case Ash.TypeResolver.ExprAnalyzer.resolve_type(module, dsl_state, expr) do
        {:ok, _type, _constraints} -> true
        _ -> false
      end
    end)
    |> then(fn {can_resolve, cannot_resolve} ->
      resolved =
        Enum.map(can_resolve, fn calc ->
          expr = get_expression!(calc)
          {:ok, type, constraints} = Ash.TypeResolver.ExprAnalyzer.resolve_type(module, dsl_state, expr)
          {calc, type, constraints}
        end)

      {resolved, cannot_resolve}
    end)
  end

  defp resolve_with_type_resolver(dsl_state, module, unresolved_calcs, all_calculations) do
    Ash.TypeResolver.ensure_started()

    # Register all known field types from this resource
    register_known_fields(module, dsl_state, all_calculations)

    # Register each unresolved :auto calc
    for calc <- unresolved_calcs do
      expr = get_expression!(calc)

      case Ash.TypeResolver.ExprAnalyzer.resolve_type(module, dsl_state, expr) do
        {:deps, deps} ->
          Ash.TypeResolver.register_auto(module, calc.name, calc, dsl_state, deps)

        {:error, reason} ->
          raise_resolution_error(module, calc.name, reason)
      end
    end

    Ash.TypeResolver.done_registering(module)

    # Wait for resolution and update DSL state
    dsl_state =
      Enum.reduce(unresolved_calcs, dsl_state, fn calc, dsl ->
        case Ash.TypeResolver.resolve(module, calc.name) do
          {:ok, type, constraints} ->
            updated_calc = %{calc | type: type, constraints: constraints}
            replace_calculation(dsl, calc, updated_calc)

          {:error, reason} ->
            raise_resolution_error(module, calc.name, reason)
        end
      end)

    {:ok, dsl_state}
  end

  defp register_known_fields(module, dsl_state, all_calculations) do
    # Register attributes
    for attr <- Ash.Resource.Info.attributes(dsl_state) do
      Ash.TypeResolver.register_known_field(module, attr.name, attr.type, attr.constraints)
    end

    # Register explicitly-typed calculations
    for calc <- all_calculations, calc.type != :auto do
      Ash.TypeResolver.register_known_field(module, calc.name, calc.type, calc.constraints)
    end

    # Register aggregates
    for agg <- Ash.Resource.Info.aggregates(dsl_state) do
      if agg.type do
        Ash.TypeResolver.register_known_field(module, agg.name, agg.type, agg.constraints || [])
      end
    end

    :ok
  end

  defp apply_resolutions(dsl_state, resolved_calcs) do
    Enum.reduce(resolved_calcs, dsl_state, fn {calc, type, constraints}, dsl ->
      updated_calc = %{calc | type: type, constraints: constraints}
      replace_calculation(dsl, calc, updated_calc)
    end)
  end

  defp replace_calculation(dsl_state, old_calc, new_calc) do
    Transformer.replace_entity(
      dsl_state,
      [:calculations],
      new_calc,
      &(&1.name == old_calc.name)
    )
  end

  defp validate_expression_calc!(calc, module) do
    case calc.calculation do
      {Ash.Resource.Calculation.Expression, _opts} ->
        :ok

      other ->
        raise Spark.Error.DslError,
          module: module,
          path: [:calculations, :calculate, calc.name],
          message:
            "`:auto` type is only supported for expression calculations (`expr(...)`), " <>
              "but calculation `#{calc.name}` uses `#{inspect(other)}`. " <>
              "Please specify an explicit type."
    end
  end

  defp get_expression!(calc) do
    {Ash.Resource.Calculation.Expression, opts} = calc.calculation
    opts[:expr]
  end

  defp raise_resolution_error(module, calc_name, reason) do
    message =
      if is_binary(reason) do
        reason
      else
        inspect(reason)
      end

    raise Spark.Error.DslError,
      module: module,
      path: [:calculations, :calculate, calc_name],
      message: "Failed to resolve `:auto` type for calculation `#{calc_name}`: #{message}"
  end
end
