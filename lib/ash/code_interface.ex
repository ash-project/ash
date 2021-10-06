defmodule Ash.CodeInterface do
  @moduledoc """
  Used to define the functions of a code interface for a resource.
  """

  @doc false
  def require_action(resource, interface) do
    action = Ash.Resource.Info.action(resource, interface.action || interface.name)

    unless action do
      raise Ash.Error.Dsl.DslError,
        module: resource,
        message:
          "The interface of #{inspect(resource)} refers to a non-existent action #{interface.action || interface.name}",
        path: [:interfaces, :interface, interface.name]
    end

    action
  end

  defmacro define_interface(api, resource) do
    quote bind_quoted: [api: api, resource: resource], generated: true, location: :keep do
      for interface <- Ash.Resource.Info.interfaces(resource) do
        action = Ash.CodeInterface.require_action(resource, interface)

        args = interface.args || []
        arg_vars = Enum.map(args, &{&1, [], Elixir})

        doc = """
        #{action.description || "Calls the #{action.name} action on the #{inspect(resource)} resource."}

        ## Options

        #{Ash.OptionsHelpers.docs(Ash.Resource.Interface.interface_options(action.type))}
        """

        case action.type do
          :read ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(
                  __MODULE__,
                  elem(__ENV__.function, 0),
                  [
                    unquote_splicing(arg_vars),
                    %{},
                    params_or_opts
                  ]
                )
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                query =
                  opts[:query]
                  |> Kernel.||(unquote(resource))
                  |> Ash.Query.for_read(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                if unquote(interface.get?) do
                  query
                  |> unquote(api).read_one(Keyword.drop(opts, [:query, :tenant]))
                  |> case do
                    {:ok, nil} ->
                      {:error, Ash.Error.Query.NotFound.exception(resource: query.resource)}

                    {:ok, result} ->
                      {:ok, result}

                    {:error, error} ->
                      {:error, error}
                  end
                else
                  unquote(api).read(query, Keyword.drop(opts, [:query, :tenant]))
                end
              end
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(
                  __MODULE__,
                  elem(__ENV__.function, 0),
                  [
                    unquote_splicing(arg_vars),
                    %{},
                    params_or_opts
                  ]
                )
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                query =
                  opts[:query]
                  |> Kernel.||(unquote(resource))
                  |> Ash.Query.for_read(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                if unquote(interface.get?) do
                  query
                  |> unquote(api).read_one!(Keyword.drop(opts, [:query, :tenant]))
                  |> case do
                    nil ->
                      raise Ash.Error.Query.NotFound, resource: query.resource

                    result ->
                      result
                  end
                else
                  unquote(api).read!(query, Keyword.drop(opts, [:query, :tenant]))
                end
              end
            end

          :create ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 2}}
            def unquote(interface.name)(
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  opts[:changeset]
                  |> Kernel.||(unquote(resource))
                  |> Ash.Changeset.for_create(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).create(changeset, Keyword.drop(opts, [:actor, :changeset, :tenant]))
              end
            end

            @doc doc
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 2}}
            # sobelow_skip ["DOS.BinToAtom"]
            def unquote(:"#{interface.name}!")(
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  (opts[:changeset] || unquote(resource))
                  |> Ash.Changeset.for_create(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).create!(changeset, Keyword.drop(opts, [:actor, :changeset]))
              end
            end

          :update ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_update(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).update(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 3}}
            def unquote(:"#{interface.name}!")(
                  record,
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_update(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).update!(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end

          :destroy ->
            @doc doc
            @dialyzer {:nowarn_function, {interface.name, Enum.count(args) + 3}}
            def unquote(interface.name)(
                  record,
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_destroy(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).destroy(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end

            @doc doc
            # sobelow_skip ["DOS.BinToAtom"]
            @dialyzer {:nowarn_function, {:"#{interface.name}!", Enum.count(args) + 3}}
            def unquote(:"#{interface.name}!")(
                  record,
                  unquote_splicing(arg_vars),
                  params_or_opts \\ %{},
                  opts \\ []
                ) do
              if opts == [] && Keyword.keyword?(params_or_opts) do
                apply(__MODULE__, elem(__ENV__.function, 0), [
                  record,
                  unquote_splicing(arg_vars),
                  %{},
                  params_or_opts
                ])
              else
                input =
                  unquote(args)
                  |> Enum.zip([unquote_splicing(arg_vars)])
                  |> Enum.reduce(params_or_opts, fn {key, value}, params_or_opts ->
                    Map.put(params_or_opts, key, value)
                  end)

                changeset =
                  record
                  |> Ash.Changeset.for_destroy(
                    unquote(action.name),
                    input,
                    Keyword.take(opts, [:actor, :tenant])
                  )

                unquote(api).destroy!(changeset, Keyword.drop(opts, [:actor, :tenant]))
              end
            end
        end
      end
    end
  end
end
