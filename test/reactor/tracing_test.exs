defmodule Ash.Test.ReactorTracingTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Domain

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      read :by_id do
        argument :id, :uuid, allow_nil?: false

        filter expr(id == ^arg(:id))
      end
    end
  end

  defmodule TracingTestReactor do
    @moduledoc false
    use Ash.Reactor

    ash do
      default_domain(Domain)
    end

    create :create_post, Post, :create do
      inputs(%{title: value("A wonderful post about Back To The Future")})
    end

    read_one :reload_post, Post, :by_id do
      inputs(%{id: result(:create_post, [:id])})
    end

    update :update_post, Post, :update do
      initial(result(:reload_post))
      inputs(%{title: value("A wonderful post about a time travel movie")})
    end

    destroy :destroy_post, Post, :destroy do
      initial(result(:update_post))
    end
  end

  describe "tracing configured with application environment" do
    setup do
      original_config = Application.get_env(:ash, :tracer, nil)
      Application.put_env(:ash, :tracer, [Ash.Tracer.Simple])

      on_exit(fn ->
        if original_config,
          do: Application.put_env(:ash, :tracer, original_config),
          else: Application.delete_env(:ash, :tracer)
      end)

      :ok
    end

    test "actions in synchronous reactors can be traced" do
      assert {:ok, :ok} = Reactor.run(TracingTestReactor, %{}, %{}, async?: false)

      assert [
               "changeset:post:create",
               "domain:post.create",
               "query:post:by_id",
               "domain:post.by_id",
               "changeset:post:update",
               "domain:post.update",
               "changeset:post:destroy",
               "domain:post.destroy"
             ] = Ash.Tracer.Simple.gather_spans() |> Enum.map(& &1.name)
    end

    test "actions in asynchronous reactors can be traced" do
      assert {:ok, :ok} = Reactor.run(TracingTestReactor, %{}, %{}, async?: true)

      assert [
               "changeset:post:create",
               "domain:post.create",
               "query:post:by_id",
               "domain:post.by_id",
               "changeset:post:update",
               "domain:post.update",
               "changeset:post:destroy",
               "domain:post.destroy"
             ] = Ash.Tracer.Simple.gather_spans() |> Enum.map(& &1.name)
    end
  end
end
