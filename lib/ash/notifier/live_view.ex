if Code.ensure_loaded?(Phoenix.LiveView) do
  defmodule Ash.Notifier.LiveView do
    def keep_live(socket, assign, callback, liveness_opts \\ []) do
      live_config = Map.get(socket.assigns, :ash_live_config, %{})
      {result, query} = callback.(socket)

      this_config = %{
        query: query,
        callback: callback,
        liveness_opts: liveness_opts
      }

      socket
      |> Phoenix.LiveView.assign(assign, result)
      |> Phoenix.LiveView.assign(:ash_live_config, Map.put(live_config, assign, this_config))
    end

    def handle_live(socket, assign, :refetch) do
      config = Map.get(socket.assigns.ash_live_config, assign)
      {result, query} = config.callback.(socket)

      new_config = Map.put(config, :query, query)
      new_full_config = Map.put(socket.assigns.ash_live_config, assign, new_config)

      socket
      |> Phoenix.LiveView.assign(assign, result)
      |> Phoenix.LiveView.assign(:ash_live_config, new_full_config)
    end

    def handle_live(socket, assign, notification) do
      config = Map.get(socket.assigns.ash_live_config, assign)

      case Ash.Notifier.Reconciler.reconcile(
             Map.get(socket.assigns, assign),
             config.query,
             notification,
             config.liveness_opts
           ) do
        :refetch ->
          {result, query} = config.callback.(socket)

          new_config = Map.put(config, :query, query)
          new_full_config = Map.put(socket.assigns.ash_live_config, assign, new_config)

          socket
          |> Phoenix.LiveView.assign(assign, result)
          |> Phoenix.LiveView.assign(:ash_live_config, new_full_config)

        _ ->
          raise "what"
      end
    end
  end
end
