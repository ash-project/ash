defmodule Ash.Test.Resource.Validation.EmbeddedResourceTest do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule ActorAndAuthorizeMustBeInContext do
    use Ash.Resource.Validation

    def validate(changeset, _opts, _) do
      case changeset.context do
        %{private: %{actor: _, authorize?: _}} ->
          :ok

        _ ->
          {:error, "not good"}
      end
    end
  end

  defmodule ActorAndAuthorizeMustBeInSourceContext do
    use Ash.Resource.Validation

    def validate(changeset, _opts, _) do
      case changeset.context do
        %{__source__: %{data: %{name: _}, context: %{private: %{actor: _, authorize?: _}}}} ->
          :ok

        _ ->
          {:error, "not good"}
      end
    end
  end

  defmodule SubSubEmbeddedResource do
    use Ash.Resource, domain: Domain, data_layer: :embedded

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: true, public?: true
    end

    validations do
      validate {ActorAndAuthorizeMustBeInSourceContext, []}
    end

    changes do
      change fn changeset, _ ->
        case changeset.context do
          %{__source__: %{data: %{name: _}}, private: %{actor: _, authorize?: _}} ->
            changeset

          _ ->
            Ash.Changeset.add_error(changeset, "uh oh!")
        end
      end
    end
  end

  defmodule SubEmbeddedResource do
    use Ash.Resource, domain: Domain, data_layer: :embedded

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: true, public?: true
      attribute :sub_sub_embedded_resource, SubSubEmbeddedResource, public?: true
    end

    validations do
      validate {ActorAndAuthorizeMustBeInSourceContext, []}
    end

    changes do
      change fn changeset, _ ->
        case changeset.context do
          %{__source__: %{data: %{name: _}}, private: %{actor: _, authorize?: _}} ->
            changeset

          _ ->
            Ash.Changeset.add_error(changeset, "uh oh!")
        end
      end
    end
  end

  defmodule EmbeddedResource do
    use Ash.Resource, domain: Domain, data_layer: :embedded

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: true, public?: true

      attribute :sub_embedded_resources, {:array, SubEmbeddedResource},
        allow_nil?: true,
        public?: true
    end

    validations do
      validate {ActorAndAuthorizeMustBeInSourceContext, []}
    end

    changes do
      change fn changeset, _ ->
        case changeset.context do
          %{__source__: %{data: %{name: _}}, private: %{actor: _, authorize?: _}} ->
            changeset

          _ ->
            Ash.Changeset.add_error(changeset, "uh oh!")
        end
      end
    end
  end

  defmodule Resource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      uuid_primary_key :id

      attribute :embedded_resource, EmbeddedResource do
        public?(true)
      end

      attribute :name, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
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

  test "changeset during validation and changes includes actor, authorize and tenant for an embedded resource" do
    # The embedded resource's validation will raise an error if the context is missing the actor or authorize? keys
    params = %{
      embedded_resource: %{
        name: "anything will trigger a validation",
        sub_embedded_resources: [%{name: "foo", sub_sub_embedded_resource: %{name: "bar"}}]
      }
    }

    Resource
    |> Ash.Changeset.for_create(:create, params,
      actor: %{},
      authorize?: true,
      tenant: "one"
    )
    |> Ash.create!()
  end

  test "changeset during validation and changes includes actor, authorize and tenant for an embedded resource used as an argument" do
    # The embedded resource's validation will raise an error if the context is missing the actor or authorize? keys
    params = %{
      embedded_resource_arg: %{
        name: "anything will trigger a validation",
        sub_embedded_resources: [%{name: "foo", sub_sub_embedded_resource: %{name: "bar"}}]
      }
    }

    assert {:ok, _} =
             %Resource{}
             |> Ash.Changeset.for_update(:update, params,
               actor: %{},
               authorize?: true,
               tenant: "one"
             )
             |> Ash.update()
  end
end
