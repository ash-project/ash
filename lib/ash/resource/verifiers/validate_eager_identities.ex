defmodule Ash.Resource.Verifiers.ValidateEagerIdentities do
  @moduledoc """
  Confirms that eager identities are not declared on a resource with no primary read.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    primary_read =
      dsl_state
      |> Verifier.get_entities([:actions])
      |> Enum.find(&(&1.type == :read && &1.primary?))

    dsl_state
    |> Verifier.get_entities([:identities])
    |> Enum.filter(&(&1.eager_check_with || &1.pre_check_with))
    |> case do
      [] ->
        :ok

      eager ->
        attributes = Verifier.get_entities(dsl_state, [:attributes])

        if primary_read do
          non_attributes =
            Enum.filter(eager, fn identity ->
              Enum.any?(identity.keys, fn key ->
                !Enum.any?(attributes, &(&1.name == key))
              end)
            end)

          case non_attributes do
            [] ->
              :ok

            [identity] ->
              {:error,
               DslError.exception(
                 path: [:identities, identity.name],
                 message:
                   "Identity #{identity.name} is declared with `eager_check_with` or `pre_check_with` but not all of the `keys` are attributes."
               )}

            identities ->
              {:error,
               DslError.exception(
                 path: [:identities],
                 message:
                   "Identities #{Enum.map_join(identities, ",", & &1.name)} are declared with `eager_check_with` or `pre_check_with` but not all of the `keys` are attributes."
               )}
          end
        else
          names = Enum.map(eager, & &1.name)

          case names do
            [name] ->
              {:error,
               DslError.exception(
                 path: [:identities, name],
                 message:
                   "Identity #{name} is declared with `eager_check_with` or `pre_check_with` but the resource has no primary read action."
               )}

            names ->
              {:error,
               DslError.exception(
                 path: [:identities],
                 message:
                   "Identities #{Enum.join(names, ",")} are declared with `eager_check_with` or `pre_check_with` but the resource has no primary read action."
               )}
          end
        end
    end
  end
end
