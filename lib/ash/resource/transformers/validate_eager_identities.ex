defmodule Ash.Resource.Transformers.ValidateEagerIdentities do
  @moduledoc """
  Confirms that eager identities are not declared on a resource with no primary read.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def after_compile?, do: true

  def transform(resource, dsl_state) do
    primary_read = Ash.Resource.Info.primary_action(resource, :read)

    dsl_state
    |> Transformer.get_entities([:identities])
    |> Enum.filter(&(&1.eager_check_with || &1.pre_check_with))
    |> case do
      [] ->
        {:ok, dsl_state}

      eager ->
        if primary_read do
          non_attributes =
            Enum.filter(eager, fn identity ->
              Enum.any?(identity.keys, &(!Ash.Resource.Info.attribute(resource, &1)))
            end)

          case non_attributes do
            [] ->
              {:ok, dsl_state}

            [identity] ->
              {:error,
               DslError.exception(
                 module: resource,
                 path: [:identities, identity.name],
                 message:
                   "Identity #{identity.name} is declared with `eager_check_with` or `pre_check_with` but not all of the `keys` are attributes."
               )}

            identities ->
              {:error,
               DslError.exception(
                 module: resource,
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
                 module: resource,
                 path: [:identities, name],
                 message:
                   "Identity #{name} is declared with `eager_check_with` or `pre_check_with` but the resource has no primary read action."
               )}

            names ->
              {:error,
               DslError.exception(
                 module: resource,
                 path: [:identities],
                 message:
                   "Identities #{Enum.join(names, ",")} are declared with `eager_check_with` or `pre_check_with` but the resource has no primary read action."
               )}
          end
        end
    end
  end
end
