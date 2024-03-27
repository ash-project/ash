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
    Updates the actor inside the Plug connection.

    The actor is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Example

        iex> actor = build_actor(%{email: "marty@1985.retro"})
        ...> conn = build_conn() |> put_private(:ash, %{actor: actor})
        ...> actor = get_actor(conn)
        %{email: "marty@1985.retro"} = actor
        ...> conn = update_actor(conn, fn actor -> Map.put(actor, :name, "Marty Retro") end)
        ...> actor = get_actor(conn)
        %{email: "marty@1985.retro", name: "Marty Retro"} = actor
        ...> conn = update_actor(conn, fn actor -> Map.delete(actor, :email) end)
        ...> actor = get_actor(conn)
        %{name: "Marty Retro"} = actor
    """
    @spec update_actor(Conn.t(), (nil | Ash.Resource.record() -> nil | Ash.Resource.record())) ::
            Conn.t()
    def update_actor(conn, callback) do
      case get_actor(conn) do
        nil ->
          conn

        actor ->
          conn
          |> set_actor(callback.(actor))
      end
    end

    @doc """
    Sets the tenant inside the Plug connection.

    The tenant is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Example

        iex> conn = build_conn() |> set_tenant("my-tenant")
        %Plug.Conn{private: %{ash: %{tenant: "my-tenant}}} = conn
    """
    @spec set_tenant(Conn.t(), Ash.ToTenant.t()) :: Conn.t()
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

        iex> conn = build_conn() |> put_private(:ash, %{tenant: "my-tenant"})
        ...> tenant = get_tenant(conn)
        "my_tenant" = tenant

        iex> conn = build_conn() |> assign(:tenant, "my-tenant")
        ...> tenant = get_tenant(conn)
        "my_tenant" = tenant
    """
    @spec get_tenant(Conn.t()) :: term()
    def get_tenant(%{assigns: %{tenant: tenant}}) when not is_nil(tenant) do
      emit_assign_warning(:tenant)

      tenant
    end

    def get_tenant(%{private: %{ash: %{tenant: tenant}}}), do: tenant
    def get_tenant(_), do: nil

    @doc """
    Sets the context inside the Plug connection.

    Context can be used to store abitrary data about the user, connection, or
    anything else you like that doesn't belong as part of the actor or tenant.

    The context is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Example

        iex> context = %{fraud_score: 0.427}
        ...> conn = build_conn() |> set_context(context)
        %Plug.Conn{private: %{ash: %{context: %{fraud_score: 0.427}}}}

    """
    @spec set_context(Conn.t(), map()) :: Conn.t()
    def set_context(conn, context) do
      ash_private =
        conn.private
        |> Map.get(:ash, %{})
        |> Map.put(:context, context)

      conn
      |> Conn.put_private(:ash, ash_private)
    end

    @doc """
    Retrieves the context from the Plug connection.

    The context is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Example

        iex> context = %{fraud_score: 0.427}
        ...> conn = build_conn() |> put_private(:ash, %{context: context})
        ...> context = get_context(conn)
        %{fraud_score: 0.427}
    """
    @spec get_context(Conn.t()) :: nil | map()
    def get_context(%{private: %{ash: %{context: context}}}), do: context
    def get_context(_), do: nil

    @doc """
    Updates the context inside the Plug connection.

    The context is stored inside the [connection's private
    fields](https://hexdocs.pm/plug/Plug.Conn.html#module-private-fields).

    ## Example

        iex> context = %{species: "Fythetropozoat"}
        ...> conn = build_conn() |> put_private(:ash, %{context: context})
        ...> context = get_context(conn)
        %{fraud_score: 0.427}
        ...> conn = update_context(conn, fn context -> Map.put(context, :location, "Barnard's Loop") end)
        ...> context = get_context(conn)
        %{species: "Fythetropozoat", location: "Barnard's Loop"}
        ...> conn = update_context(conn, fn context -> Map.delete(context, :fraud_score) end)
        ...> context = get_context(conn)
        %{location: "Barnard's Loop"}
    """
    @spec update_context(Conn.t(), (nil | map() -> nil | map())) ::
            Conn.t()
    def update_context(conn, callback) do
      case get_context(conn) do
        nil ->
          conn

        context ->
          conn
          |> set_context(callback.(context))
      end
    end
  end
end
