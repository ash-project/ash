defmodule Ash.Actions.Create.CreateReactor do
  use Reactor

  input :changeset
  input :opts

  return :select_and_load

  step :setup_changeset_and_opts, Ash.Actions.Create.Steps.SetupChangesetAndOpts do
    async? false
    argument :changeset, input(:changeset)
    argument :opts, input(:opts)
  end

  step :changeset do
    async? false
    argument :changeset, result(:setup_changeset_and_opts), transform: &Map.get(&1, :changeset)

    run(fn args, _ ->
      if args.changeset.valid? do
        {:ok, args.changeset}
      else
        {:error, args.changeset}
      end
    end)
  end

  step :check_authorization do
    async? false
    argument :changeset, result(:setup_changeset_and_opts), transform: &Map.get(&1, :changeset)
    argument :opts, result(:setup_changeset_and_opts), transform: &Map.get(&1, :opts)

    run(fn %{changeset: changeset, opts: opts} ->
      nil
      # this is basically what we need
      # but w/ more options to `Ash.Api.can` to get the right
      # kinds of exceptions out. Actually, we should just write
      # an internal `.authorize` function or something like that
      # if opts[:authorize?] do
      #   if Ash.Api.can?(changeset.api, changeset, opts[:actor],
      #        maybe_is: false,
      #        return_forbidden_errors?: true
      #      ) do
      #     {:ok, :ok}
      #   else
      #     {:error, Ash.Error.Forbidden.exception([])}
      #   end
      # else
      #   {:ok, :ok}
      # end
      {:ok, :ok}
    end)
  end

  step :create, Ash.Actions.Create.Steps.Create do
    async? false
    argument :changeset, result(:changeset)
  end

  step :select_and_load do
    async? false
    argument :create_result, result(:create)
    argument :opts, result(:setup_changeset_and_opts), transform: &Map.get(&1, :opts)

    run(fn %{
             create_result: %{changeset: changeset, result: result, notifications: notifications},
             opts: opts
           },
           _ ->
      load_opts =
        changeset.context.private
        |> Map.take([:actor, :authorize?, :tracer])
        |> Map.to_list()

      case Ash.Actions.Helpers.load(
             {:ok, result, %{notifications: notifications}},
             changeset,
             changeset.api,
             load_opts
           ) do
        {:ok, result, instructions} ->
          notification = %Ash.Notifier.Notification{
            resource: changeset.resource,
            api: changeset.api,
            actor: changeset.context.private.actor,
            action: changeset.action,
            data: result,
            changeset: changeset
          }

          result = Ash.Actions.Helpers.select(result, changeset)

          notifications = [notification | instructions.notifications]

          if opts[:return_notifications?] do
            {:ok, {result, notifications}}
          else
            remaining = Ash.Notifier.notify(notifications)

            Ash.Actions.Helpers.warn_missed!(changeset.resource, changeset.action, remaining)

            {:ok, result}
          end

        {:error, error} ->
          {:error, error}
      end
    end)
  end
end
