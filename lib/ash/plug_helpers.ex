if Code.ensure_loaded?(Plug.Conn) do
  defmodule Ash.PlugHelpers do
    @moduledoc """
    Helpers for working with the Plug connection.
    """

    alias Plug.Conn
    require Logger

    defmacrop emit_assign_warning(type) do
      quote do
        Logger.warning(fn ->
          {fun, arity} = __ENV__.function
          file = Path.relative_to_cwd(__ENV__.file)

          """
          Storing the #{unquote(type)} in conn assigns is deprecated.
            #{file}:#{__ENV__.line}: #{inspect(__ENV__.module)}.#{fun}/#{arity}
          """
        end)
      end
    end

    @doc """
    Sets the actor inside the Plug connection.

    The actor is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Example

        iex> actor = build_actor(%{email: "marty@1985.retro"})
        ...> conn = build_conn() |> set_actor(actor)
        %Plug.Conn{private: %{ash: %{actor: %{email: "marty@1985.retro"}}}} = conn

    """
    @spec set_actor(Conn.t(), Ash.Resource.record()) :: Conn.t()
    def set_actor(conn, actor) do
      ash_private =
        conn.private
        |> Map.get(:ash, %{})
        |> Map.put(:actor, actor)

      conn
      |> Conn.put_private(:ash, ash_private)
    end

    @doc """
    Retrieves the actor from the Plug connection.

    The actor is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Deprecation warning

    This function checks to see if the actor is already set in the `@actor`
    assign, and if so will emit a deprecation warning.

    This is to allow apps using the previous method a chance to update.

    Rather than setting the actor in the assigns, please use the `set_actor/2`
    method.

    ## Example

        iex> actor = build_actor(%{email: "marty@1985.retro"})
        ...> conn = build_conn() |> put_private(:ash, %{actor: actor})
        ...> actor = get_actor(conn)
        %{email: "marty@1985.retro"} = actor

        iex> actor = build_actor(%{email: "marty@1985.retro"})
        ...> conn = build_conn() |> assign(:actor, actor)
        ...> actor = get_actor(conn)
        %{email: "marty@1985.retro"} = actor
    """
    @spec get_actor(Conn.t()) :: nil | Ash.Resource.record()
    def get_actor(%{assigns: %{actor: actor}}) when not is_nil(actor) do
      emit_assign_warning(:actor)

      actor
    end

    def get_actor(%{private: %{ash: %{actor: actor}}}), do: actor
    def get_actor(_), do: nil

    @doc """
    Sets the tenant inside the Plug connection.

    The tenant is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Example

        iex> tenant = build_tenant(%{name: "Deliver-yesterday"})
        ...> conn = build_conn() |> set_tenant(tenant)
        %Plug.Conn{private: %{ash: %{tenant: %{name: "Deliver-yesterday"}}}} = conn
    """
    @spec set_tenant(Conn.t(), Ash.Resource.record()) :: Conn.t()
    def set_tenant(conn, tenant) do
      ash_private =
        conn.private
        |> Map.get(:ash, %{})
        |> Map.put(:tenant, tenant)

      conn
      |> Conn.put_private(:ash, ash_private)
    end

    @doc """
    Retrieves the tenant from the Plug connection.

    The tenant is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Deprecation warning

    This function checks to see if the tenant is already set in the `@tenant`
    assign, and if so will emit a deprecation warning.

    This is to allow apps using the previous method a chance to update.

    Rather than setting the tenant in the assigns, please use the `set_tenant/2`
    method.


    ## Example

        iex> tenant = build_tenant(%{name: "Deliver-yesterday"})
        ...> conn = build_conn() |> put_private(:ash, %{tenant: tenant})
        ...> tenant = get_tenant(conn)
        %{name: "Deliver-yesterday"} = tenant

        iex> tenant = build_tenant(%{name: "Deliver-yesterday"})
        ...> conn = build_conn() |> assign(:tenant, tenant)
        ...> tenant = get_tenant(conn)
        %{name: "Deliver-yesterday"} = tenant
    """
    @spec get_tenant(Conn.t()) :: nil | Ash.Resource.record()
    def get_tenant(%{assigns: %{tenant: tenant}}) when not is_nil(tenant) do
      emit_assign_warning(:tenant)

      tenant
    end

    def get_tenant(%{private: %{ash: %{tenant: tenant}}}), do: tenant
    def get_tenant(_), do: nil
  end
end
