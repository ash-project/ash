defmodule Ash.Actions.Create.Steps.SetupChangesetAndOpts do
  use Reactor.Step

  def run(%{changeset: changeset, opts: opts}, _, _) do
    if opts[:after_action] do
      raise ArgumentError, "The `after_action` option is no longer supported."
    end

    {changeset, opts} = Ash.Actions.Helpers.add_process_context(changeset.api, changeset, opts)

    if is_nil(changeset.action) do
      raise "hell"
    end

    upsert? =
      changeset.action.upsert? || opts[:upsert?] ||
        get_in(changeset.context, [:private, :upsert?]) || false

    upsert_identity =
      if changeset.action.upsert? do
        changeset.action.upsert_identity || opts[:upsert_identity] ||
          get_in(changeset.context, [:private, :upsert_identity])
      else
        opts[:upsert_identity] || get_in(changeset.context, [:private, :upsert_identity])
      end

    upsert_keys =
      case upsert_identity do
        nil ->
          Ash.Resource.Info.primary_key(changeset.resource)

        identity ->
          keys =
            changeset.resource
            |> Ash.Resource.Info.identities()
            |> Enum.find(&(&1.name == identity))
            |> Kernel.||(
              raise ArgumentError,
                    "No identity found for #{inspect(changeset.resource)} called #{inspect(identity)}"
            )
            |> Map.get(:keys)

          if changeset.tenant &&
               Ash.Resource.Info.multitenancy_strategy(changeset.resource) == :attribute do
            [Ash.Resource.Info.multitenancy_attribute(changeset.resource) | keys]
          else
            keys
          end
      end

    changeset =
      changeset
      |> Ash.Changeset.set_context(%{
        private: %{upsert?: upsert?, upsert_identity: upsert_identity, upsert_keys: upsert_keys}
      })
      |> Ash.Changeset.set_defaults(:create, true)
      |> Ash.Changeset.validate_multitenancy()
      |> then(fn changeset ->
        if upsert? do
          Ash.Changeset.validate_upsert_support(changeset)
        else
          changeset
        end
      end)
      |> Ash.Changeset.set_context(%{
        private: %{return_notifications?: opts[:return_notifications?] || false}
      })
      |> set_tenant_attribute()
      |> maybe_set_timeout(opts)

    {:ok,
     %{
       changeset: changeset,
       opts: opts
     }}
  end

  defp maybe_set_timeout(changeset, opts) do
    case Keyword.fetch(opts, :timeout) do
      {:ok, timeout} ->
        Ash.Changeset.timeout(changeset, timeout)

      :error ->
        changeset
    end
  end

  defp set_tenant_attribute(changeset) do
    if changeset.tenant &&
         Ash.Resource.Info.multitenancy_strategy(changeset.resource) == :attribute do
      attribute = Ash.Resource.Info.multitenancy_attribute(changeset.resource)
      {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(changeset.resource)
      attribute_value = apply(m, f, [changeset.tenant | a])

      Ash.Changeset.force_change_attribute(changeset, attribute, attribute_value)
    else
      changeset
    end
  end
end
