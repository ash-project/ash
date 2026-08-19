# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateMultitenancy do
  @moduledoc """
  Ensures that the multitenancy configuration is valid for the given resource
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Extension
  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  # sobelow_skip ["DOS.BinToAtom"]
  @impl true
  def verify(dsl_state) do
    strategy = Verifier.get_option(dsl_state, [:multitenancy], :strategy)
    attribute = Verifier.get_option(dsl_state, [:multitenancy], :attribute)

    ancestor_attributes =
      Verifier.get_option(dsl_state, [:multitenancy], :ancestor_attributes) || []

    attributes = Verifier.get_entities(dsl_state, [:attributes])
    resource = Verifier.get_persisted(dsl_state, :module)
    data_layer = Verifier.get_persisted(dsl_state, :data_layer)

    missing_ancestor_attributes =
      Enum.reject(
        ancestor_attributes,
        fn ancestor_attribute ->
          Enum.any?(attributes, &(&1.name == ancestor_attribute))
        end
      )

    cond do
      strategy == :context && data_layer &&
          not Ash.DataLayer.can?(data_layer, resource, :multitenancy) ->
        # Get location info for the multitenancy strategy option
        strategy_anno = Extension.get_opt_anno(dsl_state, [:multitenancy], :strategy)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :strategy],
           message: "Data layer does not support multitenancy",
           location: strategy_anno
         )}

      strategy == :attribute && is_nil(attribute) ->
        # Get location info for the multitenancy strategy option
        strategy_anno = Extension.get_opt_anno(dsl_state, [:multitenancy], :strategy)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :attribute],
           message:
             "Multitenancy strategy is `:attribute`, but no `attribute` is configured. " <>
               "You must set the `attribute` option to the name of an attribute to use for multitenancy.",
           location: strategy_anno
         )}

      strategy == :attribute && !Enum.any?(attributes, &(&1.name == attribute)) ->
        # Get location info for the multitenancy attribute option
        attribute_anno = Extension.get_opt_anno(dsl_state, [:multitenancy], :attribute)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :attribute],
           message: "Attribute #{attribute} used in multitenancy configuration does not exist",
           location: attribute_anno
         )}

      ancestor_attributes != [] && strategy != :attribute ->
        ancestor_attributes_anno =
          Extension.get_opt_anno(dsl_state, [:multitenancy], :ancestor_attributes)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :ancestor_attributes],
           message: "The `ancestor_attributes` option requires the `:attribute` strategy",
           location: ancestor_attributes_anno
         )}

      ancestor_attributes != [] &&
          Enum.uniq(ancestor_attributes ++ [attribute]) != ancestor_attributes ++ [attribute] ->
        ancestor_attributes_anno =
          Extension.get_opt_anno(dsl_state, [:multitenancy], :ancestor_attributes)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :ancestor_attributes],
           message:
             "The `ancestor_attributes` #{inspect(ancestor_attributes)} and the `attribute` #{attribute} contain duplicates. " <>
               "`ancestor_attributes` lists only the levels above the tenant attribute, not the tenant attribute itself.",
           location: ancestor_attributes_anno
         )}

      missing_ancestor_attributes != [] ->
        ancestor_attributes_anno =
          Extension.get_opt_anno(dsl_state, [:multitenancy], :ancestor_attributes)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :ancestor_attributes],
           message:
             "Attribute #{Enum.map_join(missing_ancestor_attributes, ", ", &to_string/1)} used in multitenancy configuration does not exist",
           location: ancestor_attributes_anno
         )}

      true ->
        :ok
    end
  end
end
