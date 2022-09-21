defmodule Ash.Resource.Transformers.ValidateManagedRelationshipOpts do
  @moduledoc """
  Confirms that all action types declared on a resource are supported by its data layer
  """
  use Spark.Dsl.Transformer

  alias Ash.Changeset.ManagedRelationshipHelpers
  alias Spark.Dsl.Transformer
  require Logger

  def after_compile?, do: true

  def transform(dsl_state) do
    relationships = Transformer.get_entities(dsl_state, [:relationships])

    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reject(&(&1.type == :read))
    |> Enum.each(fn action ->
      action.changes
      |> Enum.filter(
        &match?(%Ash.Resource.Change{change: {Ash.Resource.Change.ManageRelationship, _}}, &1)
      )
      |> Enum.each(fn %Ash.Resource.Change{change: {_, opts}} ->
        unless Enum.find(action.arguments, &(&1.name == opts[:argument])) do
          raise Spark.Error.DslError,
            path:
              [
                :actions,
                action.type,
                action.name,
                :change,
                :manage_relationship
              ] ++ Enum.uniq([opts[:argument], opts[:relationship]]),
            message: "Action #{action.name} has no argument `#{inspect(opts[:argument])}`."
        end

        relationship =
          Enum.find(relationships, &(&1.name == opts[:relationship])) ||
            raise Spark.Error.DslError,
              path:
                [
                  :actions,
                  action.type,
                  action.name,
                  :change,
                  :manage_relationship
                ] ++ Enum.uniq([opts[:argument], opts[:relationship]]),
              message: "No such relationship #{opts[:relationship]} exists."

        if ensure_compiled?(relationship) do
          try do
            opts =
              if opts[:opts][:type] == :replace do
                Logger.warn(
                  "type: :replace has been renamed to `:append_and_remove` in 2.0, and it will be removed in 2.1"
                )

                Keyword.update!(opts, :opts, fn inner_opts ->
                  Keyword.put(inner_opts, :type, :append_and_remove)
                end)
              else
                opts
              end

            manage_opts =
              if opts[:opts][:type] do
                defaults = Ash.Changeset.manage_relationship_opts(opts[:opts][:type])

                Enum.reduce(defaults, Ash.Changeset.manage_relationship_schema(), fn {key, value},
                                                                                     manage_opts ->
                  Spark.OptionsHelpers.set_default!(manage_opts, key, value)
                end)
              else
                Ash.Changeset.manage_relationship_schema()
              end

            opts = Spark.OptionsHelpers.validate!(opts[:opts], manage_opts)

            ManagedRelationshipHelpers.sanitize_opts(relationship, opts)
          rescue
            e ->
              reraise Spark.Error.DslError,
                      [
                        path:
                          [
                            :actions,
                            action.type,
                            action.name,
                            :change,
                            :manage_relationship
                          ] ++ Enum.uniq([opts[:argument], opts[:relationship]]),
                        message: """
                        The following error was raised when validating options provided to manage_relationship.

                        #{Exception.format(:error, e, __STACKTRACE__)}
                        """
                      ],
                      __STACKTRACE__
          end
        end
      end)
    end)

    {:ok, dsl_state}
  end

  defp ensure_compiled?(%Ash.Resource.Relationships.ManyToMany{
         through: through,
         destination: destination
       }) do
    Code.ensure_loaded?(through) && Code.ensure_loaded?(destination)
  end

  defp ensure_compiled?(%{destination: destination}) do
    Code.ensure_loaded?(destination)
  end
end
