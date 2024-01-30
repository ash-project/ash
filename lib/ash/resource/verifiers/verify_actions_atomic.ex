defmodule Ash.Resource.Verifiers.VerifyActionsAtomic do
  @moduledoc """
  Raises an error on update or destroy actions with `require_atomic?` set to
  true when it is known at compile time that they will not be atomic.

  Note: this currently does nothing. There is an issue w/ the way we prevent
  compile time dependencies from actions on the changes that they call.

  We can't do this check until we have a way to express that changes & validations should
  be export dependencies only, not regular compile time and/or non-depended on
  modules
  """
  use Spark.Dsl.Verifier

  def verify(_dsl) do
    # module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    # dsl
    # |> Ash.Resource.Info.actions()
    # |> Enum.filter(&(&1.type in [:update, :destroy] and &1.require_atomic?))
    # |> Enum.each(fn action ->
    #   if action.manual do
    #     raise Spark.Error.DslError,
    #       module: module,
    #       message:
    #         non_atomic_message(module, action.name, "manual actions cannot be done atomically"),
    #       path: [:actions, action.name]
    #   end

    #   action.changes
    #   |> Enum.map(fn
    #     %{change: {module, _}} ->
    #       module

    #     %{validation: {module, _}} ->
    #       module
    #   end)
    #   |> Enum.reject(fn module ->
    #     module.atomic?()
    #   end)
    #   |> case do
    #     [] ->
    #       :ok

    #     non_atomic_change_modules ->
    #       raise Spark.Error.DslError,
    #         module: module,
    #         message:
    #           non_atomic_message(
    #             module,
    #             action.name,
    #             "the changes `#{inspect(non_atomic_change_modules)}` can never be done atomically"
    #           ),
    #         path: [:actions, action.name]
    #   end
    # end)

    :ok
  end

  # defp non_atomic_message(module, action_name, reason) do
  #   "#{inspect(module)}.#{action_name} cannot be done atomically, because #{reason}"
  # end
end
