defmodule Ash.Test.Resource.Validation.EmbeddedResourceTest do
  @moduledoc false

  use ExUnit.Case, async: true

  defmodule ActorAndAuthorizeMustBeInContext do
    use Ash.Resource.Validation

    def validate(changeset, _opts) do
      # Make sure you pass actor and authorize? into your changeset for this validation to pass
      if %{private: %{actor: _, authorize?: _}} = changeset.context do
        :ok
      end
    end
  end

  defmodule ActorAndAuthorizeMustBeInSourceContext do
    use Ash.Resource.Validation

    def validate(changeset, _opts) do
      # How does this validation get the tenant, authorize? and actor values?
      # The __source__ is not present.
      # Should they be in changeset.context.private or changeset.context.private.__source__.context.private?
      if %{private: %{actor: _, authorize?: _}} = changeset.context do
        :ok
      end
    end
  end

  defmodule EmbeddedResource do
    use Ash.Resource, data_layer: :embedded

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: true
    end

    validations do
      validate {ActorAndAuthorizeMustBeInSourceContext, []}
    end

    changes do
      change fn changeset, _ ->
        # Creating or updating an embedded resource may have sideaffects that require the actor, authorize? and tenant to be set
        if %{private: %{actor: _, authorize?: _}} = changeset.context do
          changeset
        end
      end
    end
  end

  defmodule Resource do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :embedded_resource, EmbeddedResource
    end

    actions do
      defaults [:read, :create]
      default_accept [:embedded_resource]

      update :update do
        argument :embedded_resource_arg, EmbeddedResource, allow_nil?: false
      end
    end

    validations do
      validate {ActorAndAuthorizeMustBeInContext, []}
    end

  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry Resource
    end
  end

  defmodule Api do
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "changeset during validation and changes includes actor, authorize and tenant for an embedded resource" do
    # The embedded resource's validation will raise an error if the context is missing the actor or authorize? keys
    params = %{ embedded_resource: %{name: "anything will trigger a validation"} }
    assert {:ok, _} = Resource |> Ash.Changeset.for_create(:create, params, actor: %{}, authorize?: true, tenant: "one") |> Api.create()  
  end

  test "changeset during validation and changes includes actor, authorize and tenant for an embedded resource used as an argument" do
    # The embedded resource's validation will raise an error if the context is missing the actor or authorize? keys
    params = %{ embedded_resource_arg: %{name: "anything will trigger a validation"} }
    assert {:ok, _} = %Resource{} |> Ash.Changeset.for_update(:update, params, actor: %{}, authorize?: true, tenant: "one") |> Api.update()  
  end

end
