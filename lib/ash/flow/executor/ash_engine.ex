defmodule Ash.Flow.Executor.AshEngine do
  @moduledoc """
  Executes the requests using the Ash engine, which can parallelize individual steps when possible.
  """
  use Ash.Flow.Executor

  defmodule Step do
    @moduledoc false
    defstruct [:step, :input]
  end

  defmodule Flow do
    @moduledoc false
    defstruct [:steps, :flow]
  end

  def build(flow, input, _opts) do
    steps =
      flow
      |> Ash.Flow.Info.steps()
      |> Enum.map(&%Step{step: &1, input: input})
      |> unnest_transactions()
      |> expand_flows()

    {:ok,
     %Flow{
       steps: steps,
       flow: flow
     }}
  end

  defp unnest_transactions(steps) do
    Enum.map(steps, fn
      %Step{step: %Ash.Flow.Step.Transaction{steps: steps}} = step ->
        {steps, touches_resources} = do_unnest_transactions(steps)

        %{
          step
          | step: %{
              step.step
              | touches_resources: Enum.uniq(step.step.touches_resources ++ touches_resources),
                steps: steps
            }
        }

      other ->
        other
    end)
  end

  defp do_unnest_transactions(steps) do
    Enum.reduce(steps, {[], []}, fn
      %Ash.Flow.Step.Transaction{
        steps: steps,
        touches_resources: touches_resources,
        name: transaction_name
      },
      {steps_acc, touches_resources_acc} ->
        {steps, more_touches_resources} = do_unnest_transactions(steps)
        steps = remap_step_names(steps, &[transaction_name | List.wrap(&1)])

        {steps_acc ++ steps, touches_resources ++ more_touches_resources ++ touches_resources_acc}

      step, {steps_acc, touches_resources_acc} ->
        {steps_acc ++ [step], touches_resources_acc}
    end)
  end

  defp expand_flows(steps, in_transaction? \\ false) do
    Enum.flat_map(steps, fn
      %Step{
        input: input,
        step: %Ash.Flow.Step.RunFlow{
          name: name,
          flow: flow,
          input: run_flow_input
        }
      } = run_flow_step ->
        flow_steps = Ash.Flow.Info.steps(flow)

        flow_steps =
          if in_transaction? do
            do_unnest_transactions(flow_steps)
          else
            flow_steps
          end

        flow_steps
        |> Stream.map(fn step ->
          %{step | name: [name | List.wrap(step.name)]}
        end)
        |> Stream.map(fn step ->
          {input, _} = Ash.Flow.handle_input_template(run_flow_input, input)

          input = Ash.Flow.remap_result_references(input, name)
          %Step{step: step, input: input}
        end)
        |> Stream.map(fn
          %Step{step: %{input: step_input} = inner_step, input: input} = step ->
            {input, _} = Ash.Flow.handle_input_template(step_input, input)

            input = Ash.Flow.remap_result_references(input, name)

            %{step | step: %{inner_step | input: input}}

          other ->
            other
        end)
        |> Stream.map(fn
          %Step{step: %Ash.Flow.Step.Map{over: over}, input: input} = step ->
            {over, _} = Ash.Flow.handle_input_template(over, input)

            over = Ash.Flow.remap_result_references(over, name)
            %{step | step: %{step.step | over: over}}

          other ->
            other
        end)
        |> Stream.map(fn
          %Step{step: %Ash.Flow.Step.Map{output: output} = inner_step} = step ->
            new_output =
              case output do
                empty when empty in [nil, []] ->
                  empty

                {key, val} ->
                  [{key, [name | List.wrap(val)]}]

                value when not is_list(value) ->
                  [{value, [name | List.wrap(value)]}]

                values when is_list(values) ->
                  Enum.map(values, fn
                    {key, value} ->
                      {key, [name | List.wrap(value)]}

                    key ->
                      {key, [name | List.wrap(key)]}
                  end)
              end

            %{step | step: %{inner_step | output: new_output}}

          other ->
            other
        end)
        |> expand_flows(in_transaction?)
        # We add it back in here because we need to make a return value for that step available
        |> Enum.concat([run_flow_step])
        |> Enum.map(fn step ->
          %{step | step: step.step}
        end)

      %Step{step: %Ash.Flow.Step.Map{steps: map_steps} = inner_step, input: input} = step ->
        new_map_steps =
          map_steps
          |> Enum.map(&%Step{step: &1, input: input})
          |> expand_flows(in_transaction?)

        [%{step | step: %{inner_step | steps: new_map_steps}}]

      %Step{
        step: %Ash.Flow.Step.Transaction{steps: transaction_steps} = inner_step,
        input: input
      } = step ->
        new_transaction_steps =
          transaction_steps
          |> Enum.map(&%Step{step: &1, input: input})
          |> expand_flows(true)

        [%{step | step: %{inner_step | steps: new_transaction_steps}}]

      step ->
        [step]
    end)
  end

  def execute(%Flow{steps: steps, flow: flow}, _input, opts) do
    steps
    |> Enum.flat_map(fn
      %Step{
        step:
          %Ash.Flow.Step.Transaction{
            steps: transaction_steps,
            name: name,
            touches_resources: touches_resources
          } = transaction
      } ->
        depends_on_requests = transaction_dependencies(transaction_steps, steps)

        [
          Ash.Engine.Request.new(
            path: [name],
            name: "Transaction #{inspect(name)}",
            async?: Enum.any?(transaction_steps, &must_be_local?/1),
            touches_resources: touches_resources,
            error_path: [name],
            data:
              Ash.Engine.Request.resolve(depends_on_requests, fn context ->
                steps
                |> requests(
                  transaction_steps,
                  opts,
                  context: context,
                  transaction_name: transaction.name
                )
                |> case do
                  [] ->
                    {:ok, %{}}

                  [first | rest] ->
                    # only one of the requests needs to be annotated as touching the resources
                    # the transaction claims to touch, since the transaction is urn over all touched resources
                    # in all requests
                    [
                      %{
                        first
                        | touches_resources:
                            Enum.uniq(touches_resources ++ first.touches_resources)
                      }
                      | rest
                    ]
                    |> Ash.Engine.run(nil, verbose?: opts[:verbose?], transaction?: true)
                    |> case do
                      {:ok, %{data: data}} ->
                        {:ok,
                         Map.new(transaction_steps, fn step ->
                           {step.step.name, Ash.Flow.do_get_in(data, data_path(step.step))}
                         end)}

                      {:error, %Ash.Engine.Runner{errors: errors}} ->
                        {:error, Ash.Error.to_error_class(errors)}

                      {:error, error} ->
                        {:error, error}
                    end
                end
              end)
          )
        ]

      %Step{} = step ->
        requests(steps, [step], opts)
    end)
    |> Ash.Engine.run(nil, verbose?: opts[:verbose?])
    |> case do
      {:ok, %Ash.Engine.Runner{data: data}} ->
        {:ok, return_value(steps, data, Ash.Flow.Info.returns(flow))}

      {:error, %Ash.Engine.Runner{errors: errors}} ->
        {:error, Ash.Error.to_error_class(errors)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp must_be_local?(%{resource: resource, touches_resources: touches_resources}) do
    Ash.Engine.must_be_local?(%Ash.Engine.Request{
      resource: resource,
      async?: true,
      touches_resources: touches_resources
    })
  end

  defp must_be_local?(_), do: false

  defp transaction_dependencies(transaction_steps, all_steps, search_steps \\ nil) do
    search_steps
    |> Kernel.||(transaction_steps)
    |> Enum.flat_map(fn
      %Ash.Flow.Step.Map{steps: steps} ->
        transaction_dependencies(transaction_steps, all_steps, steps)

      step ->
        {_, deps} = Ash.Flow.handle_input_template(step.input, step.input)

        Enum.flat_map(deps, fn {:_result, dep} ->
          case Enum.find(transaction_steps, fn step ->
                 step.step.name == dep
               end) do
            nil ->
              dependent_step =
                Enum.find(all_steps, fn
                  %Step{step: %Ash.Flow.Step.Transaction{steps: transaction_steps}} ->
                    Enum.any?(transaction_steps, &(&1.name == dep))

                  %Step{step: step} ->
                    step.name == dep
                end)

              result_path(dependent_step)

            _step ->
              []
          end
        end)
    end)
    |> Enum.uniq()
  end

  defp return_value(steps, data, returns) do
    case returns do
      name when is_atom(name) ->
        get_return_value(steps, data, name)

      names when is_list(names) ->
        Map.new(names, fn
          {key, name} ->
            {name, get_return_value(steps, data, key)}

          name ->
            {name, get_return_value(steps, data, name)}
        end)
    end
  end

  defp get_return_value(steps, data, name) do
    case Enum.find(steps, fn
           %Step{step: %Ash.Flow.Step.Transaction{steps: steps}} ->
             Enum.any?(steps, &(&1.step.name == name))

           %Step{step: step} ->
             step.name == name
         end) do
      nil ->
        nil

      %Step{step: %Ash.Flow.Step.Transaction{name: transaction_name}} ->
        Ash.Flow.do_get_in(data, [transaction_name, :data, name])

      %Step{step: step} ->
        Ash.Flow.do_get_in(data, data_path(step))
    end
  end

  defp requests(all_steps, steps, opts, request_opts \\ []) do
    additional_context = request_opts[:context] || %{}
    transaction_name = request_opts[:transaction_name]

    steps
    |> Enum.flat_map(fn
      %Step{
        step: %Ash.Flow.Step.Map{name: name, steps: map_steps, over: over, output: output},
        input: input
      } ->
        output = output || List.last(map_steps).step.name
        # {input_depends_on =
        {over, deps} = Ash.Flow.handle_input_template(over, input)
        dep_paths = get_dep_paths(all_steps, deps, transaction_name)

        request_deps =
          Enum.map(dep_paths, fn {_, %{request_path: request_path}} -> request_path end)

        [
          Ash.Engine.Request.new(
            path: [name],
            async?: true,
            name: "#{inspect(name)}",
            error_path: List.wrap(name),
            data:
              Ash.Engine.Request.resolve(request_deps, fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results =
                  Map.new(dep_paths, fn {name, %{fetch_path: fetch_path}} ->
                    {name, Ash.Flow.do_get_in(context, fetch_path)}
                  end)

                elements =
                  over
                  |> Ash.Flow.set_dependent_values(%{
                    results: results,
                    elements: Map.get(context, :_ash_engine_elements)
                  })
                  |> Ash.Flow.handle_modifiers()
                  |> Kernel.||([])

                if Enum.empty?(elements) do
                  {:ok, []}
                else
                  case Ash.Flow.do_get_in(context, [[name, :elements]]) do
                    nil ->
                      elements
                      |> Enum.with_index()
                      |> Enum.flat_map(fn {element, i} ->
                        new_additional_context =
                          additional_context
                          |> Map.put_new(:_ash_engine_elements, %{})
                          |> Map.update!(:_ash_engine_elements, &Map.put(&1, name, element))
                          |> Map.put_new(:_ash_engine_indices, %{})
                          |> Map.update!(:_ash_engine_indices, &Map.put(&1, name, i))

                        map_steps =
                          map_steps
                          |> remap_step_names(&(List.wrap(&1) ++ [i]))

                        output_name = List.wrap(output) ++ [i]
                        output_step = Enum.find(map_steps, &(&1.step.name == output_name)).step
                        output_path = result_path(output_step)

                        all_steps
                        |> Enum.map(fn step ->
                          if step.step.name == name do
                            %{step | step: %{step.step | steps: map_steps}}
                          else
                            step
                          end
                        end)
                        |> requests(
                          map_steps,
                          opts,
                          context: new_additional_context
                        )
                        |> Kernel.++([
                          Ash.Engine.Request.new(
                            path: [[name, :elements], i],
                            error_path: [[name, :elements], i],
                            name: "Handle #{inspect(name)} index #{i}",
                            authorize?: false,
                            data:
                              Ash.Engine.Request.resolve([output_path], fn context ->
                                {:ok, Ash.Flow.do_get_in(context, output_path)}
                              end),
                            async?: true
                          )
                        ])
                      end)
                      |> case do
                        [] ->
                          {:ok, []}

                        requests ->
                          {:requests, Enum.map(requests, &{&1, :data})}
                      end

                    elements ->
                      {:ok,
                       elements
                       |> Enum.sort_by(&elem(&1, 0))
                       |> Enum.map(&elem(&1, 1))
                       |> Enum.map(&Map.get(&1 || %{}, :data))}
                  end
                end
              end)
          )
        ]

      %Step{
        step: %Ash.Flow.Step.Custom{
          name: name,
          input: custom_input,
          custom: {mod, opts},
          async?: async?
        },
        input: input
      } ->
        {custom_input, deps} = Ash.Flow.handle_input_template(custom_input, input)
        dep_paths = get_dep_paths(all_steps, deps, transaction_name)

        request_deps =
          Enum.map(dep_paths, fn {_, %{request_path: request_path}} -> request_path end)

        [
          Ash.Engine.Request.new(
            authorize?: false,
            async?: async?,
            name: "Run custom step #{inspect(name)}",
            path: [name],
            error_path: List.wrap(name),
            data:
              Ash.Engine.Request.resolve(request_deps, fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results =
                  Map.new(dep_paths, fn {name, %{fetch_path: fetch_path}} ->
                    {name, Ash.Flow.do_get_in(context, fetch_path)}
                  end)

                custom_input =
                  custom_input
                  |> Ash.Flow.set_dependent_values(%{
                    results: results,
                    elements: Map.get(context, :_ash_engine_elements)
                  })
                  |> Ash.Flow.handle_modifiers()

                mod.run(custom_input, opts, context)
              end)
          )
        ]

      %Step{step: %Ash.Flow.Step.RunFlow{name: name, flow: flow}} ->
        flow_steps = Ash.Flow.Info.steps(flow)

        depends_on_requests =
          flow
          |> Ash.Flow.Info.returns()
          |> List.wrap()
          |> Enum.map(fn
            {key, _} ->
              key

            other ->
              other
          end)
          |> Enum.map(fn key ->
            step = Enum.find(flow_steps, &(&1.name == key))

            result_path(%{step | name: [name | List.wrap(step.name)]})
          end)

        [
          Ash.Engine.Request.new(
            path: [name],
            name: "Coalesce return value for #{inspect(name)}",
            async?: false,
            error_path: List.wrap(name),
            data:
              Ash.Engine.Request.resolve(depends_on_requests, fn context ->
                case Ash.Flow.Info.returns(flow) do
                  key when is_atom(key) ->
                    {:ok, get_flow_return_value(all_steps, [name, key], context)}

                  list when is_list(list) ->
                    list =
                      Enum.map(list, fn
                        {key, val} ->
                          {key, val}

                        key ->
                          {key, key}
                      end)

                    {:ok,
                     Map.new(list, fn {key, val} ->
                       {val, get_flow_return_value(all_steps, [name, key], context)}
                     end)}
                end
              end)
          )
        ]

      %Step{step: %Ash.Flow.Step.Read{} = read, input: input} ->
        %{
          action: action,
          api: api,
          name: name,
          resource: resource,
          input: action_input,
          get?: get?
        } = read

        %{
          action: action,
          action_input: action_input,
          dep_paths: dep_paths,
          request_deps: request_deps
        } =
          action_request_info(
            all_steps,
            resource,
            action,
            action_input,
            input,
            transaction_name
          )

        Ash.Actions.Read.as_requests([name], resource, api, action,
          error_path: List.wrap(name),
          authorize?: opts[:authorize?],
          actor: opts[:actor],
          query_dependencies: request_deps,
          get?: get? || action.get?,
          query_input: fn context ->
            context = Ash.Helpers.deep_merge_maps(context, additional_context)

            results =
              Map.new(dep_paths, fn {name, %{fetch_path: fetch_path}} ->
                {name, Ash.Flow.do_get_in(context, fetch_path)}
              end)

            action_input
            |> Ash.Flow.set_dependent_values(%{
              results: results,
              elements: Map.get(context, :_ash_engine_elements)
            })
            |> Ash.Flow.handle_modifiers()
          end
        )

      %Step{step: %Ash.Flow.Step.Create{} = create, input: input} ->
        %{
          action: action,
          api: api,
          name: name,
          resource: resource,
          input: action_input
        } = create

        %{
          action: action,
          action_input: action_input,
          dep_paths: dep_paths,
          request_deps: request_deps
        } =
          action_request_info(
            all_steps,
            resource,
            action,
            action_input,
            input,
            transaction_name
          )

        Ash.Actions.Create.as_requests([name], resource, api, action,
          error_path: List.wrap(name),
          authorize?: opts[:authorize?],
          actor: opts[:actor],
          changeset_dependencies: request_deps,
          changeset_input: fn context ->
            context = Ash.Helpers.deep_merge_maps(context, additional_context)

            results =
              Map.new(dep_paths, fn {name, %{fetch_path: fetch_path}} ->
                {name, Ash.Flow.do_get_in(context, fetch_path)}
              end)

            action_input
            |> Ash.Flow.set_dependent_values(%{
              results: results,
              elements: Map.get(context, :_ash_engine_elements)
            })
            |> Ash.Flow.handle_modifiers()
          end
        )

      %Step{step: %Ash.Flow.Step.Update{} = update, input: input} ->
        %{
          action: action,
          api: api,
          name: name,
          resource: resource,
          input: action_input,
          record: record
        } = update

        %{
          action: action,
          action_input: action_input,
          dep_paths: dep_paths,
          request_deps: request_deps
        } =
          action_request_info(
            all_steps,
            resource,
            action,
            action_input,
            input,
            transaction_name
          )

        get_request =
          get_request(
            record,
            input,
            all_steps,
            transaction_name,
            name,
            resource,
            additional_context
          )

        [
          get_request
          | Ash.Actions.Update.as_requests([name], resource, api, action,
              error_path: List.wrap(name),
              authorize?: opts[:authorize?],
              actor: opts[:actor],
              changeset_dependencies: [[name, :fetch, :data] | request_deps],
              skip_on_nil_record?: true,
              record: fn context ->
                Ash.Flow.do_get_in(context, [name, :fetch, :data])
              end,
              changeset_input: fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results =
                  Map.new(dep_paths, fn {name, %{fetch_path: fetch_path}} ->
                    {name, Ash.Flow.do_get_in(context, fetch_path)}
                  end)

                action_input
                |> Ash.Flow.set_dependent_values(%{
                  results: results,
                  elements: Map.get(context, :_ash_engine_elements)
                })
                |> Ash.Flow.handle_modifiers()
              end
            )
        ]

      %Step{step: %Ash.Flow.Step.Destroy{} = destroy, input: input} ->
        %{
          action: action,
          api: api,
          name: name,
          resource: resource,
          input: action_input,
          record: record
        } = destroy

        %{
          action: action,
          action_input: action_input,
          dep_paths: dep_paths,
          request_deps: request_deps
        } =
          action_request_info(
            all_steps,
            resource,
            action,
            action_input,
            input,
            transaction_name
          )

        get_request =
          get_request(
            record,
            input,
            all_steps,
            transaction_name,
            name,
            resource,
            additional_context
          )

        [
          get_request
          | Ash.Actions.Destroy.as_requests([name], resource, api, action,
              error_path: List.wrap(name),
              authorize?: opts[:authorize?],
              actor: opts[:actor],
              changeset_dependencies: [[name, :fetch, :data] | request_deps],
              skip_on_nil_record?: true,
              record: fn context ->
                Ash.Flow.do_get_in(context, [name, :fetch, :data])
              end,
              changeset_input: fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results =
                  Map.new(dep_paths, fn {name, %{fetch_path: fetch_path}} ->
                    {name, Ash.Flow.do_get_in(context, fetch_path)}
                  end)

                action_input
                |> Ash.Flow.set_dependent_values(%{
                  results: results,
                  elements: Map.get(context, :_ash_engine_elements)
                })
                |> Ash.Flow.handle_modifiers()
              end
            )
        ]
    end)
  end

  defp remap_step_names(steps, fun) do
    step_names =
      Enum.map(steps, fn step ->
        step.step.name
      end)

    steps
    |> Enum.map(fn
      %Step{step: %{name: name} = inner_step} = step ->
        %{step | step: %{inner_step | name: fun.(name)}}
        |> remap_step_input(step_names, fun)
    end)
    |> Enum.map(fn
      %Step{step: %{steps: inner_steps}} = step ->
        %{step | step: %{step.step | steps: remap_step_names(inner_steps, fun)}}

      other ->
        other
    end)
  end

  defp remap_step_input(%{step: %{input: input} = inner_step} = step, step_names, fun) do
    new_input =
      Ash.Flow.remap_result_references(input, fn name ->
        if name in step_names do
          fun.(name)
        else
          name
        end
      end)

    %{step | step: %{inner_step | input: new_input}}
  end

  defp remap_step_input(other, _, _), do: other

  defp get_flow_return_value(steps, name, data) do
    case Enum.find(steps, fn
           %Step{step: %Ash.Flow.Step.Transaction{steps: steps}} ->
             Enum.any?(steps, &(&1.name == name))

           %Step{step: step} ->
             step.name == name
         end) do
      nil ->
        nil

      %Step{step: %Ash.Flow.Step.Transaction{name: transaction_name}} ->
        Ash.Flow.do_get_in(data, [transaction_name, :data, name])

      %Step{step: step} ->
        Ash.Flow.do_get_in(data, result_path(%{step | name: name}))
    end
  end

  defp get_request(
         record,
         input,
         all_steps,
         transaction_name,
         name,
         resource,
         additional_context
       ) do
    {record, record_deps} = Ash.Flow.handle_input_template(record, input)

    get_request_dep_paths = get_dep_paths(all_steps, record_deps, transaction_name)

    get_request_deps =
      Enum.map(get_request_dep_paths, fn {_, %{request_path: request_path}} ->
        request_path
      end)

    Ash.Engine.Request.new(
      path: [name, :fetch],
      name: "Fetch record for #{inspect(name)}",
      error_path: List.wrap(name) ++ [:fetch],
      async?: must_be_local?(%{resource: resource}),
      resource: resource,
      authorize?: false,
      data:
        Ash.Engine.Request.resolve(get_request_deps, fn context ->
          context = Ash.Helpers.deep_merge_maps(context, additional_context)

          results =
            Map.new(get_request_dep_paths, fn {name, %{fetch_path: fetch_path}} ->
              {name, Ash.Flow.do_get_in(context, fetch_path)}
            end)

          record
          |> Ash.Flow.set_dependent_values(%{
            results: results,
            elements: Map.get(context, :_ash_engine_elements)
          })
          |> Ash.Flow.handle_modifiers()
          |> case do
            %^resource{} = record ->
              {:ok, record}

            nil ->
              {:ok, nil}

            value ->
              {:error, "Invalid record #{inspect(value)}"}
          end
        end)
    )
  end

  defp action_request_info(
         all_steps,
         resource,
         action,
         action_input,
         input,
         transaction_name
       ) do
    action =
      Ash.Resource.Info.action(resource, action) ||
        raise "No such action #{action} for #{resource}"

    {action_input, deps} = Ash.Flow.handle_input_template(action_input, input)

    dep_paths = get_dep_paths(all_steps, deps, transaction_name)
    request_deps = Enum.map(dep_paths, fn {_, %{request_path: request_path}} -> request_path end)

    %{
      action: action,
      action_input: action_input,
      dep_paths: dep_paths,
      request_deps: request_deps
    }
  end

  defp get_dep_paths(all_steps, deps, transaction_name) do
    dep_paths(deps, all_steps, transaction_name)
  end

  defp dep_paths(deps, steps, transaction_name) do
    deps
    |> Enum.reduce(%{}, fn
      {:_result, dep}, acc ->
        case Enum.find(steps, fn
               %Step{step: %Ash.Flow.Step.Transaction{steps: steps}} ->
                 Enum.any?(steps, &(&1.step.name == dep))

               %Step{step: %{name: ^dep}} ->
                 true

               _ ->
                 false
             end) do
          nil ->
            acc

          %Step{step: %Ash.Flow.Step.Transaction{name: name, steps: transaction_steps}} ->
            if transaction_name && name == transaction_name do
              inner_step = Enum.find(transaction_steps, &(&1.step.name == dep))
              result_path = result_path(inner_step.step)
              Map.put(acc, dep, %{fetch_path: result_path, request_path: result_path})
            else
              Map.put(acc, dep, %{fetch_path: [name, :data, dep], request_path: [name, :data]})
            end

          %Step{step: step} ->
            result_path = result_path(step)
            Map.put(acc, dep, %{fetch_path: result_path, request_path: result_path})
        end

      _, acc ->
        acc
    end)
  end

  defp result_path(%Ash.Flow.Step.Read{name: name}) do
    [name, :data, :data]
  end

  defp result_path(%Ash.Flow.Step.Create{name: name}) do
    [name, :commit, :data]
  end

  defp result_path(%Ash.Flow.Step.Update{name: name}) do
    [name, :commit, :data]
  end

  defp result_path(%Ash.Flow.Step.RunFlow{name: name}) do
    [name, :data]
  end

  defp result_path(%Ash.Flow.Step.Map{name: name}) do
    [name, :data]
  end

  defp result_path(%Ash.Flow.Step.Custom{name: name}) do
    [name, :data]
  end

  defp data_path(%Ash.Flow.Step.Read{name: name}) do
    [name, :data]
  end

  defp data_path(%Ash.Flow.Step.Create{name: name}) do
    [name, :commit]
  end

  defp data_path(%Ash.Flow.Step.Update{name: name}) do
    [name, :commit]
  end

  defp data_path(%Ash.Flow.Step.RunFlow{name: name}) do
    [name]
  end

  defp data_path(%Ash.Flow.Step.Custom{name: name}) do
    [name]
  end

  defp data_path(%Ash.Flow.Step.Map{name: name}) do
    [name]
  end
end
