# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Can do
  @moduledoc """
  Contains the Ash.can function logic.
  """

  require Ash.Query
  require Logger

  @type subject ::
          Ash.Query.t()
          | Ash.Changeset.t()
          | Ash.ActionInput.t()
          | {Ash.Resource.t(), atom | Ash.Resource.Actions.action()}
          | {Ash.Resource.t(), atom | Ash.Resource.Actions.action(), input :: map}
          | {Ash.Resource.record(), atom | Ash.Resource.Actions.action()}
          | {Ash.Resource.record(), atom | Ash.Resource.Actions.action(), input :: map}

  @doc """
  Returns whether an actor can perform an action, query, or changeset.

  You should prefer to use `Ash.can?/3` over this module, directly.

  Can raise an exception if return_forbidden_error is truthy in opts or there's an error.
  """
  @spec can?(subject(), Ash.Domain.t(), Ash.Resource.record(), Keyword.t()) ::
          boolean() | no_return()
  def can?(action_or_query_or_changeset, domain, actor, opts \\ []) do
    opts =
      opts
      |> Keyword.put_new(:maybe_is, true)
      |> Keyword.put_new(:filter_with, :filter)

    case can(action_or_query_or_changeset, domain, actor, opts) do
      {:ok, :maybe} ->
        opts[:maybe_is]

      {:ok, result} ->
        result

      {:ok, true, _} ->
        true

      {:ok, false, error} ->
        if opts[:return_forbidden_error?] do
          raise Ash.Error.to_ash_error(error)
        else
          false
        end

      {:error, error} ->
        raise Ash.Error.to_ash_error(error)
    end
  end

  @doc """
  Returns a an ok tuple if the actor can perform the action, query, or changeset,
  an error tuple if an error happens, and a ok tuple with maybe if maybe is set to true
  or not set.

  You should prefer to use `Ash.can/3` over this module, directly.

  Note: `is_maybe` is set to `true`, if not set.
  """
  @spec can(subject(), Ash.Domain.t(), Ash.actor() | Ash.Scope.t(), Keyword.t()) ::
          {:ok, boolean() | :maybe}
          | {:ok, boolean(), term()}
          | {:ok, boolean(), Ash.Changeset.t(), Ash.Query.t()}
          | {:error, Ash.Error.t()}
  def can(action_or_query_or_changeset, domain, actor_or_scope, opts \\ []) do
    opts = Keyword.put_new(opts, :maybe_is, :maybe)
    opts = Keyword.put_new(opts, :run_queries?, true)
    opts = Keyword.put_new(opts, :filter_with, :filter)

    {actor, opts} =
      if is_struct(actor_or_scope) and Ash.Scope.ToOpts.impl_for(actor_or_scope) do
        opts
        |> Keyword.put(:scope, actor_or_scope)
        |> Ash.Actions.Helpers.apply_scope_to_opts()
        |> Keyword.pop(:actor)
      else
        {actor_or_scope, opts}
      end

    {resource, action_or_query_or_changeset, input, opts} =
      case resource_subject_input(action_or_query_or_changeset, domain, actor, opts) do
        {resource, action_or_query_or_changeset, input, new_opts} ->
          {resource, action_or_query_or_changeset, input, Keyword.merge(new_opts, opts)}

        {resource, action_or_query_or_changeset, input} ->
          {resource, action_or_query_or_changeset, input, opts}
      end

    subject =
      case action_or_query_or_changeset do
        %Ash.ActionInput{} = action_input ->
          action_input
          |> Ash.ActionInput.set_tenant(opts[:tenant] || action_input.tenant)
          |> Ash.ActionInput.set_context(%{private: %{actor: actor}})

        %Ash.Query{} = query ->
          query
          |> Ash.Query.set_tenant(opts[:tenant] || query.tenant)
          |> Ash.Query.set_context(%{private: %{actor: actor}})

        %Ash.Changeset{} = changeset ->
          changeset
          |> Ash.Changeset.set_tenant(opts[:tenant] || changeset.tenant)
          |> Ash.Changeset.set_context(%{private: %{actor: actor}})

        %{type: :update, name: name} ->
          if opts[:data] do
            Ash.Changeset.for_update(opts[:data], name, input,
              actor: actor,
              tenant: opts[:tenant]
            )
          else
            resource
            |> struct()
            |> Ash.Changeset.for_update(name, input, actor: actor, tenant: opts[:tenant])
          end

        %{type: :create, name: name} ->
          Ash.Changeset.for_create(resource, name, input, actor: actor, tenant: opts[:tenant])

        %{type: :read, name: name} ->
          Ash.Query.for_read(resource, name, input, actor: actor, tenant: opts[:tenant])

        %{type: :destroy, name: name} ->
          if opts[:data] do
            Ash.Changeset.for_destroy(opts[:data], name, input,
              actor: actor,
              tenant: opts[:tenant]
            )
          else
            resource
            |> struct()
            |> Ash.Changeset.for_destroy(name, input, actor: actor, tenant: opts[:tenant])
          end

        %{type: :action, name: name} ->
          Ash.ActionInput.for_action(resource, name, input, actor: actor, tenant: opts[:tenant])

        _ ->
          raise ArgumentError,
            message: "Invalid action/query/changeset \"#{inspect(action_or_query_or_changeset)}\""
      end

    if opts[:validate?] && !subject.valid? do
      {:ok, false, Ash.Error.to_error_class(subject.errors)}
    else
      subject = %{subject | domain: domain}

      pre_flight? = Keyword.get(opts, :pre_flight?, true)
      reuse_values? = Keyword.get(opts, :reuse_values?, false)

      opts =
        if pre_flight? && !reuse_values? && opts[:data] do
          fields = [:__metadata__ | Enum.to_list(Ash.Resource.Info.attribute_names(resource))]

          Keyword.update!(opts, :data, fn data ->
            data
            |> List.wrap()
            |> Enum.map(fn record ->
              struct(resource, Map.take(record, fields))
            end)
          end)
        else
          opts
        end

      subject =
        case subject do
          %Ash.Query{} ->
            Ash.Query.set_context(subject, %{private: %{pre_flight_authorization?: pre_flight?}})

          %Ash.Changeset{} ->
            changeset =
              Ash.Changeset.set_context(subject, %{
                private: %{pre_flight_authorization?: pre_flight?}
              })

            if pre_flight? && !reuse_values? && is_struct(changeset.data, resource) do
              fields = [:__metadata__ | Enum.to_list(Ash.Resource.Info.attribute_names(resource))]

              %{changeset | data: struct(resource, Map.take(changeset.data, fields))}
            else
              changeset
            end

          %Ash.ActionInput{} ->
            Ash.ActionInput.set_context(subject, %{
              private: %{pre_flight_authorization?: pre_flight?}
            })
        end
        |> Ash.Subject.set_context(%{
          private: %{authorizer_log?: opts[:log?] || false}
        })

      case Ash.Domain.Info.resource(domain, resource) do
        {:ok, _} ->
          domain
          |> run_check(actor, subject, opts)
          |> alter_source(domain, actor, subject, opts)

        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp resource_subject_input(action_or_query_or_changeset, domain, actor, opts) do
    case action_or_query_or_changeset do
      %Ash.Query{} = query ->
        {query.resource, query, nil}

      %Ash.Changeset{} = changeset ->
        {changeset.resource, changeset, nil}

      %Ash.ActionInput{} = input ->
        {input.resource, input, nil}

      {resource, name} when is_atom(name) and is_atom(resource) ->
        action =
          Ash.Resource.Info.action(resource, name) ||
            raise ArgumentError, "No such action #{name} on #{inspect(resource)}"

        resource_subject_input(
          {resource, action, %{}},
          domain,
          actor,
          opts
        )

      {resource, name, input} when is_atom(name) and is_atom(resource) and not is_nil(name) ->
        action =
          Ash.Resource.Info.action(resource, name) ||
            raise ArgumentError, "No such action #{name} on #{inspect(resource)}"

        resource_subject_input(
          {resource, action, input},
          domain,
          actor,
          opts
        )

      {%Ash.Query{} = query, name} ->
        query
        |> Ash.Query.for_read(name, %{})
        |> resource_subject_input(domain, actor, opts)

      {%Ash.Changeset{} = changeset, name} ->
        changeset
        |> Ash.Changeset.for_action(name, %{})
        |> resource_subject_input(domain, actor, opts)

      {%Ash.ActionInput{} = input, name} ->
        input
        |> Ash.ActionInput.for_action(name, %{})
        |> resource_subject_input(domain, actor, opts)

      {%Ash.Query{} = query, name, input} ->
        query
        |> Ash.Query.for_read(name, input)
        |> resource_subject_input(domain, actor, opts)

      {%Ash.Changeset{} = changeset, name, input} ->
        changeset
        |> Ash.Changeset.for_action(name, input)
        |> resource_subject_input(domain, actor, opts)

      {%Ash.ActionInput{} = input, name, action_input} ->
        input
        |> Ash.ActionInput.for_action(name, action_input)
        |> resource_subject_input(domain, actor, opts)

      {%resource{} = record, name}
      when is_atom(name) and is_atom(resource) and not is_nil(name) ->
        action =
          Ash.Resource.Info.action(resource, name) ||
            raise ArgumentError, "No such action #{name} on #{inspect(resource)}"

        resource_subject_input(
          {record, action, %{}},
          domain,
          actor,
          opts
        )

      {%resource{} = record, name, input}
      when is_atom(name) and is_atom(resource) and not is_nil(name) ->
        action =
          Ash.Resource.Info.action(resource, name) ||
            raise ArgumentError, "No such action #{name} on #{inspect(resource)}"

        resource_subject_input(
          {record, action, input},
          domain,
          actor,
          opts
        )

      {resource, %struct{} = action}
      when struct in [
             Ash.Resource.Actions.Create,
             Ash.Resource.Actions.Read,
             Ash.Resource.Actions.Update,
             Ash.Resource.Actions.Destroy,
             Ash.Resource.Actions.Action
           ] ->
        resource_subject_input({resource, action, %{}}, domain, actor, opts)

      {%resource{} = record, %Ash.Resource.Actions.Read{} = action, input} ->
        {resource,
         Ash.Query.for_read(resource, action.name, input,
           domain: domain,
           tenant: opts[:tenant],
           actor: actor
         ), input, data: [record]}

      {%resource{}, %Ash.Resource.Actions.Action{} = action, input} ->
        {resource,
         Ash.ActionInput.for_action(resource, action.name, input,
           domain: domain,
           tenant: opts[:tenant],
           actor: actor
         ), input}

      {%resource{}, %Ash.Resource.Actions.Create{} = action, input} ->
        {resource,
         Ash.Changeset.for_create(resource, action.name, input,
           domain: domain,
           tenant: opts[:tenant],
           actor: actor
         ), input}

      {%resource{} = record, %struct{} = action, input}
      when struct in [
             Ash.Resource.Actions.Update,
             Ash.Resource.Actions.Destroy
           ] ->
        {resource,
         Ash.Changeset.for_action(record, action.name, input,
           domain: domain,
           tenant: opts[:tenant],
           actor: actor
         ), input}

      {resource, %Ash.Resource.Actions.Read{} = action, input} ->
        {resource,
         Ash.Query.for_read(resource, action.name, input,
           domain: domain,
           tenant: opts[:tenant],
           actor: actor
         ), input}

      {resource, %Ash.Resource.Actions.Action{} = action, input} ->
        {resource,
         Ash.ActionInput.for_action(resource, action.name, input,
           domain: domain,
           tenant: opts[:tenant],
           actor: actor
         ), input}

      {resource, %Ash.Resource.Actions.Create{} = action, input} ->
        {resource,
         Ash.Changeset.for_create(resource, action.name, input,
           domain: domain,
           tenant: opts[:tenant],
           actor: actor
         ), input}

      {resource, %struct{} = action, input}
      when struct in [
             Ash.Resource.Actions.Update,
             Ash.Resource.Actions.Destroy
           ] ->
        {resource, action, input}

      {resource, action} ->
        raise ArgumentError, """
        If providing an update or destroy action, you must provide a record to update or destroy.

        Got: #{inspect({resource, action})}
        """
    end
  end

  defp alter_source({:ok, true, query}, domain, actor, %Ash.Changeset{} = subject, opts) do
    case alter_source({:ok, true}, domain, actor, subject, Keyword.put(opts, :base_query, query)) do
      {:ok, true, new_subject} -> {:ok, true, new_subject, query}
      other -> other
    end
  end

  defp alter_source({:ok, true, query}, domain, actor, _subject, opts) do
    alter_source({:ok, true}, domain, actor, query, opts)
  end

  defp alter_source({:ok, true}, domain, actor, subject, opts) do
    if opts[:alter_source?] do
      subject.resource
      |> Ash.Resource.Info.authorizers()
      |> case do
        [] ->
          {:ok, true, subject}

        authorizers ->
          authorizers
          |> Enum.reduce(
            {:ok, true, subject},
            fn authorizer, {:ok, true, subject} ->
              authorizer_state =
                authorizer.initial_state(
                  actor,
                  subject.resource,
                  subject.action,
                  domain
                )

              context = %{
                actor: actor,
                tenant: subject.to_tenant,
                domain: domain,
                resource: subject.resource,
                query: nil,
                changeset: nil,
                action_input: nil,
                subject: subject
              }

              context =
                case subject do
                  %Ash.Query{} -> Map.put(context, :query, subject)
                  %Ash.Changeset{} -> Map.put(context, :changeset, subject)
                  %Ash.ActionInput{} -> Map.put(context, :action_input, subject)
                end

              case subject do
                %Ash.Query{} = query ->
                  alter_query(query, authorizer, authorizer_state, context, opts)

                %Ash.Changeset{} = changeset ->
                  context = Map.put(context, :changeset, changeset)

                  with {:ok, changeset, authorizer_state} <-
                         Ash.Authorizer.add_calculations(
                           authorizer,
                           changeset,
                           authorizer_state,
                           context
                         ) do
                    if opts[:base_query] do
                      case alter_query(
                             opts[:base_query],
                             authorizer,
                             authorizer_state,
                             context,
                             opts
                           ) do
                        {:ok, true, query} ->
                          {:ok, true, changeset, query}

                        other ->
                          other
                      end
                    else
                      {:ok, true, changeset}
                    end
                  end

                %Ash.ActionInput{} = subject ->
                  {:ok, true, subject}
              end
            end
          )
      end
    else
      {:ok, true}
    end
  end

  defp alter_source(other, _, _, _, _), do: other

  defp alter_query(query, authorizer, authorizer_state, context, opts) do
    context = Map.put(context, :query, query)

    with {:ok, query, _} <-
           Ash.Authorizer.add_calculations(
             authorizer,
             query,
             authorizer_state,
             context
           ),
         {:ok, new_filter} <-
           Ash.Authorizer.alter_filter(
             authorizer,
             authorizer_state,
             query.filter,
             context
           ),
         {:ok, hydrated} <-
           Ash.Filter.hydrate_refs(new_filter, %{
             resource: query.resource,
             public?: false
           }),
         hydrated <- fill_template(hydrated, context, opts),
         {:ok, new_sort} <-
           Ash.Authorizer.alter_sort(
             authorizer,
             authorizer_state,
             query.sort,
             context
           ) do
      {:ok, true, %{query | filter: hydrated, sort: new_sort}}
    end
  end

  defp fill_template(expr, context, opts) do
    {:ok, expr} =
      Ash.Filter.hydrate_refs(expr, %{
        resource: context.resource,
        public?: false
      })

    if opts[:atomic_changeset] do
      Ash.Expr.fill_template(
        expr,
        actor: context.actor,
        tenant: opts[:atomic_changeset].to_tenant,
        args: opts[:atomic_changeset].arguments,
        context: opts[:atomic_changeset].context,
        changeset: opts[:atomic_changeset]
      )
    else
      expr
    end
  end

  defp run_check(domain, actor, subject, opts) do
    authorizers =
      Ash.Resource.Info.authorizers(subject.resource)
      |> Enum.map(fn authorizer ->
        authorizer_state =
          authorizer.initial_state(
            actor,
            subject.resource,
            subject.action,
            domain
          )

        context = %{
          actor: actor,
          tenant: subject.to_tenant,
          domain: domain,
          resource: subject.resource,
          query: nil,
          changeset: nil,
          action_input: nil,
          subject: subject
        }

        context =
          case subject do
            %Ash.Query{} -> Map.put(context, :query, subject)
            %Ash.Changeset{} -> Map.put(context, :changeset, subject)
            %Ash.ActionInput{} -> Map.put(context, :action_input, subject)
          end

        {authorizer, authorizer_state, context}
      end)

    base_query =
      case subject do
        %Ash.Query{} = query ->
          opts[:base_query] || query

        _ ->
          opts[:base_query]
      end

    case authorizers do
      [] ->
        if opts[:log?] do
          Logger.info("No authorizers present on #{inspect(subject.resource)}")
        end

        {:ok, true}

      authorizers ->
        authorizers
        |> Enum.reduce_while(
          {false, base_query, []},
          fn {authorizer, authorizer_state, context}, {_authorized?, query, authorizers} ->
            case authorizer.strict_check(authorizer_state, context) do
              {:error, %{class: :forbidden} = e} when is_exception(e) ->
                {:halt, {false, e, {authorizer, authorizer_state, context}}}

              {:error, error} ->
                {:halt, {:error, authorizer, error}}

              {:authorized, authorizer_state} ->
                {:cont, {true, query, [{authorizer, authorizer_state, context} | authorizers]}}

              :forbidden ->
                {:halt,
                 {false, Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state),
                  {authorizer, authorizer_state, context}}}

              _ when not is_nil(context.action_input) ->
                raise """
                Cannot use filter or runtime checks with generic actions

                Failed when authorizing #{inspect(subject.resource)}.#{subject.action.name}
                """

              {:filter, authorizer_state, filter} ->
                filter = fill_template(filter, context, opts)

                {:cont,
                 {true,
                  apply_filter(
                    query,
                    subject,
                    domain,
                    filter,
                    authorizer,
                    authorizer_state,
                    opts
                  ), [{authorizer, authorizer_state, context} | authorizers]}}

              {:filter, filter} ->
                filter = fill_template(filter, context, opts)

                {:cont,
                 {true,
                  apply_filter(
                    query,
                    subject,
                    domain,
                    filter,
                    authorizer,
                    authorizer_state,
                    opts
                  ), [{authorizer, authorizer_state, context} | authorizers]}}

              {:continue, authorizer_state} ->
                if opts[:no_check?] do
                  {:halt,
                   opts[:on_must_pass_strict_check] ||
                     {:error, {authorizer, authorizer_state, context},
                      Ash.Authorizer.exception(
                        authorizer,
                        :must_pass_strict_check,
                        authorizer_state
                      )}}
                else
                  if opts[:alter_source?] || !match?(%Ash.Query{}, subject) do
                    query_with_hook =
                      Ash.Query.authorize_results(
                        or_query(query, subject.resource, domain, subject),
                        fn query, results ->
                          context = Map.merge(context, %{data: results, query: query})

                          case authorizer.check(authorizer_state, context) do
                            :authorized ->
                              {:ok, results}

                            {:error, :forbidden, authorizer_state} ->
                              {:error,
                               Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)}

                            {:error, error} ->
                              {:error, error}

                            {:data, data} ->
                              {:ok, data}
                          end
                        end
                      )

                    {:cont,
                     {true, query_with_hook,
                      [{authorizer, authorizer_state, context} | authorizers]}}
                  else
                    if opts[:maybe_is] == false do
                      {:halt,
                       {false, Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state),
                        {authorizer, authorizer_state, context}}}
                    else
                      {:halt,
                       {:maybe, nil, [{authorizer, authorizer_state, context} | authorizers]}}
                    end
                  end
                end

              {:filter_and_continue, filter, authorizer_state} ->
                filter = fill_template(filter, context, opts)

                if opts[:no_check?] || !match?(%Ash.Query{}, subject) do
                  {:error, {authorizer, authorizer_state, context},
                   Ash.Authorizer.exception(
                     authorizer,
                     :must_pass_strict_check,
                     authorizer_state
                   )}
                else
                  if opts[:alter_source?] do
                    query_with_hook =
                      query
                      |> apply_filter(
                        subject,
                        domain,
                        filter,
                        authorizer,
                        authorizer_state,
                        opts
                      )
                      |> Ash.Query.authorize_results(fn query, results ->
                        context = Map.merge(context, %{data: results, query: query})

                        case authorizer.check(authorizer_state, context) do
                          :authorized ->
                            {:ok, results}

                          {:error, :forbidden, authorizer_state} ->
                            {:error,
                             Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)}

                          {:error, error} ->
                            {:error, error}

                          {:data, data} ->
                            {:ok, data}
                        end
                      end)

                    {:cont,
                     {true, query_with_hook,
                      [{authorizer, authorizer_state, context} | authorizers]}}
                  else
                    if opts[:maybe_is] == false do
                      {:halt,
                       {false, Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state),
                        authorizer}}
                    else
                      {:halt,
                       {:maybe, nil, [{authorizer, authorizer_state, context} | authorizers]}}
                    end
                  end
                end
            end
          end
        )
        |> case do
          {:error, _authorizer, error} ->
            {:error, error}

          {true, nil, _} ->
            {:ok, true}

          {true, query, authorizers} when not is_nil(query) ->
            if opts[:run_queries?] do
              run_queries(subject, actor, opts, authorizers, query)
            else
              if opts[:alter_source?] do
                {:ok, true, query}
              else
                {:ok, :maybe}
              end
            end

          {false, error, authorizer} ->
            if opts[:return_forbidden_error?] do
              {:ok, false, error || authorizer_exception([authorizer])}
            else
              {:ok, false}
            end

          {:maybe, _v, authorizers} ->
            if opts[:maybe_is] == false && opts[:return_forbidden_error?] do
              {:ok, false, authorizer_exception(authorizers)}
            else
              {:ok, opts[:maybe_is]}
            end
        end
    end
  end

  defp apply_filter(query, subject, domain, filter, authorizer, authorizer_state, opts) do
    resource = subject.resource
    filter = Ash.Filter.parse!(resource, filter).expression

    case opts[:filter_with] || :filter do
      :filter ->
        Ash.Query.filter(or_query(query, resource, domain, subject), ^filter)

      :error ->
        Ash.Query.filter(
          or_query(query, resource, domain, subject),
          if ^filter do
            true
          else
            error(Ash.Error.Forbidden.Placeholder, %{
              authorizer: ^inspect(authorizer)
            })
          end
        )
        |> Ash.Query.set_context(%{
          private: %{authorizer_state: %{authorizer => authorizer_state}}
        })
    end
  end

  defp run_queries(subject, actor, opts, authorizers, query) do
    case subject do
      %Ash.Query{tenant: tenant} ->
        if opts[:data] do
          data = List.wrap(opts[:data])

          pkey = Ash.Resource.Info.primary_key(query.resource)
          pkey_values = Enum.map(data, &Map.take(&1, pkey))

          if Enum.any?(pkey_values, fn pkey_value ->
               pkey_value |> Map.values() |> Enum.any?(&is_nil/1)
             end) do
            {:ok, :maybe}
          else
            query
            |> Ash.Query.do_filter(or: pkey_values)
            |> Ash.Query.select([])
            |> Ash.Query.set_tenant(tenant)
            |> Ash.Actions.Read.add_calc_context_to_query(
              actor,
              true,
              query.tenant,
              opts[:tracer],
              query.domain,
              expand?: false,
              parent_stack: Ash.Actions.Read.parent_stack_from_context(subject.context),
              source_context: subject.context
            )
            |> Ash.Query.data_layer_query()
            |> case do
              {:ok, data_layer_query} ->
                data_layer_query
                |> Ash.DataLayer.run_query(query.resource)
                |> Ash.Actions.Helpers.rollback_if_in_transaction(query.resource, query)
                |> case do
                  {:ok, results} ->
                    case Ash.Actions.Read.run_authorize_results(query, results) do
                      {:ok, results} ->
                        if Enum.count(results) == Enum.count(data) do
                          {:ok, true}
                        else
                          if opts[:return_forbidden_error?] do
                            {:ok, false, authorizer_exception(authorizers)}
                          else
                            {:ok, false}
                          end
                        end

                      {:error, error} ->
                        {:error, error}
                    end

                  {:error, error} ->
                    {:error, error}
                end

              {:error, error} ->
                {:error, error}
            end
          end
        else
          {:ok, true}
        end

      %Ash.Changeset{data: data, action_type: type, resource: resource, tenant: tenant} =
          changeset
      when type in [:update, :destroy] ->
        pkey = Ash.Resource.Info.primary_key(resource)
        pkey_value = Map.take(data, pkey)

        query =
          Map.update!(query, :filter, fn filter ->
            Ash.Expr.fill_template(
              filter,
              actor: actor,
              actor: changeset.to_tenant,
              args: changeset.arguments,
              context: changeset.context,
              changeset: changeset
            )
          end)

        if pkey_value |> Map.values() |> Enum.any?(&is_nil/1) do
          {:ok, :maybe}
        else
          query
          |> Ash.Query.do_filter(pkey_value)
          |> Ash.Query.set_tenant(tenant)
          |> Ash.Query.select([])
          |> Ash.Actions.Read.add_calc_context_to_query(
            actor,
            true,
            query.tenant,
            opts[:tracer],
            query.domain,
            expand?: false,
            parent_stack: Ash.Actions.Read.parent_stack_from_context(subject.context),
            source_context: subject.context
          )
          |> Ash.Query.data_layer_query()
          |> case do
            {:ok, data_layer_query} ->
              data_layer_query
              |> Ash.DataLayer.run_query(resource)
              |> Ash.Actions.Helpers.rollback_if_in_transaction(query.resource, query)
              |> case do
                {:ok, results} ->
                  case Ash.Actions.Read.run_authorize_results(query, results) do
                    {:ok, []} ->
                      if opts[:return_forbidden_error?] do
                        {:ok, false, authorizer_exception(authorizers)}
                      else
                        {:ok, false}
                      end

                    {:ok, [_]} ->
                      {:ok, true}

                    {:error, error} ->
                      if opts[:return_forbidden_error?] do
                        {:ok, false, error}
                      else
                        {:ok, false}
                      end
                  end

                {:error, error} ->
                  {:error, error}

                _ ->
                  if opts[:return_forbidden_error?] do
                    {:ok, false, authorizer_exception(authorizers)}
                  else
                    {:ok, false}
                  end
              end

            {:error, error} ->
              {:error, error}
          end
        end

      %Ash.Changeset{} ->
        raise Ash.Error.Forbidden.CannotFilterCreates, filter: query.filter
    end
  end

  defp or_query(query, resource, domain, subject) do
    query || Ash.Query.set_context(Ash.Query.new(resource, domain: domain), subject.context)
  end

  defp authorizer_exception([{authorizer, authorizer_state, _context}]) do
    Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)
  end

  defp authorizer_exception(authorizers) do
    authorizers
    |> Enum.map(&authorizer_exception([&1]))
    |> Ash.Error.to_error_class()
  end
end
