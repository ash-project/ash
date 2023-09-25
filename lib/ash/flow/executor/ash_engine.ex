defmodule Ash.Flow.Executor.AshEngine do
  @moduledoc """
  Executes the requests using the Ash engine, which can parallelize individual steps when possible.
  """
  use Ash.Flow.Executor
  require Logger
  require Ash.Tracer
  require Ash.Flow.StepHelpers

  defmodule Step do
    @moduledoc false
    defstruct [:step, :input]
  end

  defmodule Flow do
    @moduledoc false
    defstruct [:steps, :flow, :returns]
  end

  def build(flow, input, opts) do
    steps =
      flow
      |> Ash.Flow.Info.steps()
      |> to_steps(input, opts)
      |> hydrate_flows(opts)

    {:ok,
     %Flow{
       steps: steps,
       flow: flow,
       returns: Ash.Flow.Info.returns(flow)
     }}
  end

  defp to_steps(steps, input, opts) do
    Enum.map(steps, fn step ->
      step =
        case step do
          %{steps: steps} = step ->
            step =
              if Map.has_key?(step, :output) && is_nil(step.output) do
                if last_step = List.last(steps) do
                  %{step | output: last_step.name}
                else
                  step
                end
              else
                step
              end

            %{step | steps: to_steps(steps, input, opts)}

          %{tenant: _tenant} = step ->
            %{step | tenant: Keyword.get(opts, :tenant)}

          step ->
            step
        end

      %Step{step: step, input: input}
    end)
  end

  defp hydrate_flows(steps, opts) do
    Enum.flat_map(steps, fn
      %Step{
        input: input,
        step: %Ash.Flow.Step.RunFlow{
          name: name,
          flow: flow,
          input: run_flow_input,
          wait_for: wait_for,
          halt_if: halt_if
        }
      } = run_flow_step ->
        {run_flow_input, _} = Ash.Flow.Template.handle_input_template(run_flow_input, input)
        {:ok, %{steps: hydrated_steps}} = build(flow, run_flow_input, opts)

        built_steps =
          hydrated_steps
          |> then(fn steps ->
            if wait_for do
              map_steps(steps, fn
                %{wait_for: nil} = step ->
                  %{step | wait_for: wait_for}

                %{wait_for: step_wait_for} = step
                when is_list(step_wait_for) and is_list(wait_for) ->
                  %{step | wait_for: wait_for ++ step_wait_for}

                %{wait_for: step_wait_for} = step when is_list(step_wait_for) ->
                  %{step | wait_for: [wait_for | step_wait_for]}

                %{wait_for: step_wait_for} = step ->
                  %{step | wait_for: [wait_for, step_wait_for]}
              end)
            else
              steps
            end
          end)
          |> then(fn steps ->
            if halt_if do
              map_steps(steps, fn
                %{halt_if: nil} = step ->
                  %{step | halt_if: halt_if}

                %{halt_if: step_halt_if} = step ->
                  %{step | halt_if: Ash.Flow.StepHelpers.expr(^step_halt_if or ^halt_if)}
              end)
            else
              steps
            end
          end)
          |> remap_step_names(fn nested_name ->
            List.wrap(name) ++ List.wrap(nested_name)
          end)
          |> handle_input_templates()

        Enum.concat(built_steps, [
          %{run_flow_step | step: %{run_flow_step.step | built: built_steps}}
        ])
        |> handle_input_templates()

      %Step{step: %{steps: steps} = inner_step} = step ->
        [%{step | step: %{inner_step | steps: hydrate_flows(steps, opts)}}]
        |> handle_input_templates()

      step ->
        [step] |> handle_input_templates()
    end)
  end

  @deps_keys [
    :input,
    :over,
    :record,
    :wait_for,
    :halt_if,
    :tenant,
    :condition,
    :resource,
    :api,
    :action
  ]

  defp handle_input_templates(run_flow_steps) do
    run_flow_steps
    |> map_outer_steps(fn %Step{step: step, input: input} = outer_step ->
      new_step =
        Enum.reduce(@deps_keys, step, fn key, step ->
          case Map.fetch(step, key) do
            {:ok, value} ->
              {new_value, _} = Ash.Flow.Template.handle_input_template(value, input)

              Map.put(step, key, new_value)

            :error ->
              step
          end
        end)

      %{outer_step | step: new_step}
    end)
  end

  def execute(%Flow{steps: steps, flow: flow}, _input, opts) do
    tracer =
      case opts[:tracer] do
        nil ->
          List.wrap(Application.get_env(:ash, :tracer))

        tracer when is_list(tracer) ->
          tracer ++ List.wrap(Application.get_env(:ash, :tracer))

        tracer ->
          [tracer | List.wrap(Application.get_env(:ash, :tracer))]
      end
      |> Enum.uniq()

    steps
    |> Enum.flat_map(&requests(flow, steps, &1, opts))
    |> resume(opts[:resume])
    |> case do
      {:error, error} ->
        {:error, error}

      {:ok, requests} ->
        requests
        |> Ash.Engine.run(
          actor: opts[:actor],
          verbose?: opts[:verbose?],
          timeout: opts[:timeout],
          authorize?: Keyword.get(opts, :authorize?, true),
          name: inspect(flow),
          failure_mode: :continue,
          tracer: tracer
        )
        |> case do
          {:ok, %Ash.Engine{data: data, resource_notifications: resource_notifications}} ->
            runner_metadata = %{data: data}

            {:ok, return_value(steps, data, Ash.Flow.Info.returns(flow)),
             %{notifications: resource_notifications, runner_metadata: runner_metadata}}

          {:error,
           %Ash.Engine{errors: errors, data: data, resource_notifications: resource_notifications}} ->
            {:error, %{runner_metadata: %{data: data}, notifications: resource_notifications},
             Ash.Error.to_error_class(errors)}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp resume(requests, nil), do: {:ok, requests}

  defp resume(requests, %Ash.Flow.Result{runner_metadata: %{data: data}}) do
    {:ok,
     Enum.map(requests, fn request ->
       case Ash.Flow.do_fetch_in(data, request.path) do
         {:ok, result} ->
           %{request | completion: true, data: result}

         :error ->
           request
       end
     end)}
  end

  defp resume(_, _) do
    {:error, "Could not resume flow because engine data was not present!"}
  end

  defp must_be_local?(%{resource: resource, touches_resources: touches_resources}) do
    Ash.Engine.must_be_local?(%Ash.Engine.Request{
      resource: resource,
      async?: true,
      touches_resources: touches_resources
    })
  end

  defp must_be_local?(_), do: false

  defp transaction_dependencies(
         transaction_name,
         transaction_steps,
         all_steps
       ) do
    Enum.flat_map(transaction_steps, fn step ->
      deps =
        step.step
        |> get_all_deps()
        |> Enum.flat_map(fn {:_result, dep} ->
          case find_step_dep(all_steps, dep, transaction_name, true) do
            nil ->
              []

            step ->
              [result_path(step)]
          end
        end)

      case step do
        %{step: %{steps: steps}} ->
          deps ++ transaction_dependencies(transaction_name, steps, all_steps)

        _ ->
          deps
      end
    end)
  end

  def deps_keys, do: @deps_keys

  defp get_all_deps(step) do
    Enum.flat_map(@deps_keys, fn key ->
      case Map.fetch(step, key) do
        {:ok, value} ->
          {_, deps} = Ash.Flow.Template.handle_input_template(value, %{})
          deps

        :error ->
          []
      end
    end)
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
    case find_step_dep(steps, name, nil) do
      nil ->
        nil

      %Ash.Flow.Step.Transaction{name: ^name, output: output} ->
        get_return_value(steps, data, output)

      %Ash.Flow.Step.Transaction{name: transaction_name} ->
        Ash.Flow.do_get_in(data, [transaction_name, name])

      step ->
        Ash.Flow.do_get_in(data, data_path(step))
    end
  end

  defp halt_if(halt_if, reason, step, results, context, otherwise) do
    halt_if =
      halt_if
      |> Ash.Flow.Template.set_dependent_values(%{
        results: results,
        elements: Map.get(context, :_ash_engine_elements)
      })
      |> Ash.Flow.handle_modifiers()

    if halt_if do
      {:error, Ash.Error.Flow.Halted.exception(step: step, reason: reason)}
    else
      otherwise.()
    end
  end

  defp requests(flow, all_steps, step, opts, request_opts \\ []) do
    additional_context = request_opts[:context] || %{}
    transaction_name = request_opts[:transaction_name]

    case step do
      %Step{
        step: %Ash.Flow.Step.Debug{
          name: name,
          input: input,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        }
      } ->
        {input, deps} = Ash.Flow.Template.handle_input_template(input, %{})
        {_, wait_for_deps} = Ash.Flow.Template.handle_input_template(wait_for, %{})
        {_, halt_if_deps} = Ash.Flow.Template.handle_input_template(halt_if, %{})

        dep_paths =
          get_dep_paths(all_steps, deps, transaction_name, wait_for_deps ++ halt_if_deps)

        request_deps = dependable_request_paths(dep_paths)
        id = System.unique_integer()

        [
          Ash.Engine.Request.new(
            path: [name],
            name: inspect(name),
            async?: false,
            error_path: [name],
            data:
              Ash.Engine.Request.resolve(request_deps, fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results = results(dep_paths, context)

                halt_if(halt_if, halt_reason, name, results, context, fn ->
                  input =
                    input
                    |> Ash.Flow.Template.set_dependent_values(%{
                      results: results,
                      elements: Map.get(context, :_ash_engine_elements)
                    })
                    |> Ash.Flow.handle_modifiers()

                  output = """
                  Debug Output for: #{inspect(name)} | #{id}

                  #{inspect(input)}
                  """

                  IO.puts(output)

                  {:ok, output}
                end)
              end)
          )
        ]

      %Step{
        step:
          %Ash.Flow.Step.Transaction{
            steps: transaction_steps,
            name: name,
            resource: resource,
            wait_for: wait_for,
            halt_if: halt_if,
            halt_reason: halt_reason,
            touches_resources: touches_resources
          } = transaction
      } ->
        request_name = "Transaction #{inspect(name)}"

        maybe_dynamic(
          [resource, touches_resources],
          request_name,
          [name],
          %{},
          all_steps,
          transaction_name,
          additional_context,
          fn resource, touches_resources ->
            depends_on_requests =
              transaction_dependencies(name, transaction_steps, all_steps) |> Enum.uniq()

            {_, wait_for_deps} = Ash.Flow.Template.handle_input_template(wait_for, %{})
            {_, halt_if_deps} = Ash.Flow.Template.handle_input_template(halt_if, %{})
            dep_paths = get_dep_paths(all_steps, [], name, wait_for_deps ++ halt_if_deps)

            request_deps = dependable_request_paths(dep_paths)

            touches_resources =
              if resource do
                [resource | touches_resources || []]
              else
                touches_resources || []
              end
              |> List.flatten()
              |> Enum.filter(&(&1 && is_atom(&1)))

            if touches_resources == [] do
              Logger.error("""
              No resources configured for transaction #{inspect(name)}.
              """)
            end

            [
              Ash.Engine.Request.new(
                path: [name],
                name: request_name,
                async?: Enum.any?(transaction_steps, &must_be_local?/1),
                touches_resources: touches_resources,
                error_path: [name],
                data:
                  Ash.Engine.Request.resolve(depends_on_requests ++ request_deps, fn context ->
                    context = Ash.Helpers.deep_merge_maps(context, additional_context)

                    results = results(dep_paths, context)

                    halt_if(halt_if, halt_reason, name, results, context, fn ->
                      transaction_steps
                      |> Enum.flat_map(
                        &requests(flow, all_steps, &1, opts,
                          context: context,
                          transaction_name: transaction.name
                        )
                      )
                      # TODO: Can we resume a transaction? For now, you cannot.
                      # Either the whole thing fails or the whole thing succeeds.
                      |> case do
                        [] ->
                          {:ok, %{}}

                        [first | rest] ->
                          # only one of the requests needs to be annotated as touching the resources
                          # the transaction claims to touch, since the transaction is run over all touched resources
                          # in all requests
                          [
                            %{
                              first
                              | touches_resources:
                                  Enum.uniq(touches_resources ++ first.touches_resources)
                            }
                            | rest
                          ]
                          |> Ash.Engine.run(
                            transaction_reason: %{
                              type: :flow_transaction,
                              metadata: %{
                                step_name: name,
                                flow: flow
                              }
                            },
                            authorize?: context[:authorize?],
                            resource: resource,
                            name: "Transaction #{inspect(name)}",
                            verbose?: context[:verbose?],
                            tracer: context[:tracer],
                            actor: context[:actor],
                            failure_mode: :stop,
                            transaction?: true
                          )
                          |> case do
                            {:ok, %{data: data, resource_notifications: notifications}} ->
                              if opts[:return_notifications?] do
                                {:ok,
                                 Map.new(transaction_steps, fn step ->
                                   {step.step.name,
                                    Ash.Flow.do_get_in(data, data_path(step.step))}
                                 end), %{notifications: notifications}}
                              else
                                {:ok,
                                 Map.new(transaction_steps, fn step ->
                                   {step.step.name,
                                    Ash.Flow.do_get_in(data, data_path(step.step))}
                                 end)}
                              end

                            {:error, %Ash.Engine{errors: errors}} ->
                              {:error, Ash.Error.to_error_class(errors)}

                            {:error, error} ->
                              {:error, error}
                          end
                      end
                    end)
                  end)
              )
            ]
          end
        )

      %Step{
        step: %Ash.Flow.Step.Branch{
          name: name,
          steps: branch_steps,
          output: output,
          condition: condition,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        },
        input: input
      } ->
        output = output || List.last(branch_steps).step.name

        {condition, deps} = Ash.Flow.Template.handle_input_template(condition, input)
        {_, wait_for_deps} = Ash.Flow.Template.handle_input_template(wait_for, input)
        {_, halt_if_deps} = Ash.Flow.Template.handle_input_template(halt_if, input)

        dep_paths =
          get_dep_paths(all_steps, deps, transaction_name, wait_for_deps ++ halt_if_deps)

        request_deps = dependable_request_paths(dep_paths)

        [
          Ash.Engine.Request.new(
            path: [name],
            async?: true,
            name: "#{inspect(name)}",
            error_path: List.wrap(name),
            data:
              Ash.Engine.Request.resolve(request_deps, fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results = results(dep_paths, context)

                halt_if(halt_if, halt_reason, name, results, context, fn ->
                  execute? =
                    condition
                    |> Ash.Flow.Template.set_dependent_values(%{
                      results: results,
                      elements: Map.get(context, :_ash_engine_elements)
                    })
                    |> Ash.Flow.handle_modifiers()

                  output_step = Enum.find(branch_steps, &(&1.step.name == output))

                  if is_nil(output_step) do
                    raise "#{inspect(output)} not found in #{inspect(step_names(branch_steps))}"
                  end

                  output_step = output_step.step
                  output_path = result_path(output_step)
                  result = Ash.Flow.do_fetch_in(context, output_path)

                  if execute? && match?({:ok, _}, result) do
                    result
                  else
                    if execute? do
                      all_steps_with_branch_steps =
                        all_steps
                        |> update_step(name, fn step ->
                          %{step | step: %{step.step | steps: branch_steps}}
                        end)

                      {:requests,
                       branch_steps
                       |> Enum.flat_map(
                         &requests(flow, all_steps_with_branch_steps, &1, opts,
                           context: context,
                           transaction_name: transaction_name
                         )
                       ), [output_path]}
                    else
                      requests =
                        branch_steps
                        |> step_names()
                        |> Enum.map(fn name ->
                          Ash.Engine.Request.new(
                            authorize?: false,
                            async?: false,
                            name: "Set nil result for #{inspect(name)}",
                            path: [name],
                            data: {:ok, nil}
                          )
                        end)

                      {:ok, nil,
                       %{
                         requests: requests
                       }}
                    end
                  end
                end)
              end)
          )
        ]

      %Step{
        step: %Ash.Flow.Step.Map{
          name: name,
          steps: map_steps,
          over: over,
          output: output,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        },
        input: input
      } ->
        output = output || List.last(map_steps).step.name

        {over, deps} = Ash.Flow.Template.handle_input_template(over, input)
        {_, wait_for_deps} = Ash.Flow.Template.handle_input_template(wait_for, input)
        {_, halt_if_deps} = Ash.Flow.Template.handle_input_template(halt_if, input)

        dep_paths =
          get_dep_paths(all_steps, deps, transaction_name, wait_for_deps ++ halt_if_deps)

        request_deps = dependable_request_paths(dep_paths)

        [
          Ash.Engine.Request.new(
            path: [name],
            async?: true,
            name: "#{inspect(name)}",
            error_path: List.wrap(name),
            data:
              Ash.Engine.Request.resolve(request_deps, fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results = results(dep_paths, context)

                halt_if(halt_if, halt_reason, name, results, context, fn ->
                  elements =
                    over
                    |> Ash.Flow.Template.set_dependent_values(%{
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
                            |> remap_step_names(&set_index(&1, i))

                          output_name = set_index(output, i)

                          output_step = Enum.find(map_steps, &(&1.step.name == output_name))

                          if is_nil(output_step) do
                            raise "#{inspect(output_name)} not found in #{inspect(step_names(map_steps))}"
                          end

                          output_step = output_step.step
                          output_path = result_path(output_step)

                          all_steps_with_map_steps =
                            all_steps
                            |> update_step(name, fn step ->
                              %{step | step: %{step.step | steps: map_steps}}
                            end)

                          map_steps
                          |> Enum.flat_map(
                            &requests(flow, all_steps_with_map_steps, &1, opts,
                              context: new_additional_context,
                              transaction_name: transaction_name
                            )
                          )
                          |> Kernel.++([
                            {Ash.Engine.Request.new(
                               path: [[name, :elements], i],
                               error_path: [[name, :elements], i],
                               name: "Handle #{inspect(name)} index #{i}",
                               authorize?: false,
                               additional_context: new_additional_context,
                               data:
                                 Ash.Engine.Request.resolve([output_path], fn context ->
                                   {:ok, Ash.Flow.do_get_in(context, output_path)}
                                 end),
                               async?: true
                             ), :data}
                          ])
                        end)
                        |> case do
                          [] ->
                            {:ok, []}

                          requests ->
                            {:requests, requests}
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
              end)
          )
        ]

      %Step{
        step: %Ash.Flow.Step.Custom{
          name: name,
          input: custom_input,
          custom: {mod, opts},
          async?: async?,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        },
        input: input
      } ->
        {custom_input, deps} = Ash.Flow.Template.handle_input_template(custom_input, input)
        {_, wait_for_deps} = Ash.Flow.Template.handle_input_template(wait_for, input)
        {_, halt_if_deps} = Ash.Flow.Template.handle_input_template(halt_if, input)

        dep_paths =
          get_dep_paths(all_steps, deps, transaction_name, wait_for_deps ++ halt_if_deps)

        request_deps = dependable_request_paths(dep_paths)

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

                results = results(dep_paths, context)

                halt_if(halt_if, halt_reason, name, results, context, fn ->
                  custom_input =
                    custom_input
                    |> Ash.Flow.Template.set_dependent_values(%{
                      results: results,
                      elements: Map.get(context, :_ash_engine_elements)
                    })
                    |> Ash.Flow.handle_modifiers()

                  context_arg = Map.take(context, [:actor, :authorize?, :async?, :verbose?])

                  Ash.Tracer.span :custom_flow_step,
                                  "custom step #{inspect(mod)}",
                                  opts[:tracer] do
                    metadata = %{
                      flow_short_name: Ash.Flow.Info.short_name(flow),
                      name: inspect(name),
                      step: inspect(mod)
                    }

                    Ash.Tracer.set_metadata(opts[:tracer], :custom_flow_step, metadata)

                    Ash.Tracer.telemetry_span [:ash, :flow, :custom_step], metadata do
                      mod.run(custom_input, opts, context_arg)
                    end
                  end
                end)
              end)
          )
        ]

      %Step{step: %Ash.Flow.Step.RunFlow{name: name, flow: flow, built: built}} ->
        # No need to do anything with `wait_for` here, because
        # `wait_for` is pushed down into all child steps when we hydrate
        result_dependencies =
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
            {:_result, List.wrap(name) ++ List.wrap(key)}
          end)

        # If you want the result of the run flow step, you only get it
        # when all parts of that flow are complete.
        dep_paths =
          get_dep_paths(
            all_steps,
            result_dependencies,
            transaction_name,
            step_names(built)
          )

        request_deps = dependable_request_paths(dep_paths)

        [
          Ash.Engine.Request.new(
            path: [name],
            name: "Build return value for #{inspect(name)}",
            async?: false,
            error_path: List.wrap(name),
            data:
              Ash.Engine.Request.resolve(request_deps, fn context ->
                case Ash.Flow.Info.returns(flow) do
                  key when is_atom(key) ->
                    {:ok,
                     get_flow_return_value(
                       all_steps,
                       List.wrap(name) ++ [key],
                       context,
                       transaction_name
                     )}

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
                       {val,
                        get_flow_return_value(
                          all_steps,
                          List.wrap(name) ++ [key],
                          context,
                          transaction_name
                        )}
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
          tenant: tenant,
          get?: get?,
          not_found_error?: not_found_error?,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        } = read

        List.wrap(
          maybe_dynamic(
            [resource, action],
            name,
            [name, :data],
            input,
            all_steps,
            transaction_name,
            additional_context,
            fn resource, action ->
              %{
                action: action,
                action_input: action_input,
                resource: resource,
                api: api,
                dep_paths: dep_paths,
                tenant: tenant,
                request_deps: request_deps
              } =
                action_request_info(
                  all_steps,
                  resource,
                  action,
                  api,
                  action_input,
                  input,
                  transaction_name,
                  tenant,
                  [wait_for, halt_if]
                )

              Ash.Actions.Read.as_requests([name], resource, api, action,
                error_path: List.wrap(name),
                authorize?: opts[:authorize?],
                actor: opts[:actor],
                tracer: opts[:tracer],
                query_dependencies: request_deps,
                get?: get? || action.get?,
                not_found_error?: not_found_error?,
                tenant: fn context ->
                  context = Ash.Helpers.deep_merge_maps(context, additional_context)
                  results = results(dep_paths, context)

                  tenant
                  |> Ash.Flow.Template.set_dependent_values(%{
                    results: results,
                    elements: Map.get(context, :_ash_engine_elements)
                  })
                  |> Ash.Flow.handle_modifiers()
                end,
                modify_query: fn query, context ->
                  context = Ash.Helpers.deep_merge_maps(context, additional_context)
                  results = results(dep_paths, context)

                  halt_if(halt_if, halt_reason, name, results, context, fn -> query end)
                end,
                query_input: fn context ->
                  context = Ash.Helpers.deep_merge_maps(context, additional_context)

                  results = results(dep_paths, context)

                  action_input
                  |> Ash.Flow.Template.set_dependent_values(%{
                    results: results,
                    elements: Map.get(context, :_ash_engine_elements)
                  })
                  |> Ash.Flow.handle_modifiers()
                end
              )
            end
          )
        )

      %Step{step: %Ash.Flow.Step.Create{} = create, input: input} ->
        %{
          action: action,
          api: api,
          name: name,
          resource: resource,
          input: action_input,
          tenant: tenant,
          wait_for: wait_for,
          halt_if: halt_if,
          upsert?: upsert?,
          upsert_identity: upsert_identity
        } = create

        List.wrap(
          maybe_dynamic(
            [resource, action],
            name,
            [name, :commit],
            input,
            all_steps,
            transaction_name,
            additional_context,
            fn resource, action ->
              %{
                action: action,
                resource: resource,
                api: api,
                action_input: action_input,
                dep_paths: dep_paths,
                tenant: tenant,
                request_deps: request_deps
              } =
                action_request_info(
                  all_steps,
                  resource,
                  action,
                  api,
                  action_input,
                  input,
                  transaction_name,
                  tenant,
                  [wait_for, halt_if]
                )

              Ash.Actions.Create.as_requests([name], resource, api, action,
                error_path: List.wrap(name),
                authorize?: opts[:authorize?],
                actor: opts[:actor],
                tracer: opts[:tracer],
                upsert?: upsert?,
                upsert_identity: upsert_identity,
                changeset_dependencies: request_deps,
                tenant: fn context ->
                  context = Ash.Helpers.deep_merge_maps(context, additional_context)
                  results = results(dep_paths, context)

                  tenant
                  |> Ash.Flow.Template.set_dependent_values(%{
                    results: results,
                    elements: Map.get(context, :_ash_engine_elements)
                  })
                  |> Ash.Flow.handle_modifiers()
                end,
                changeset_input: fn context ->
                  context = Ash.Helpers.deep_merge_maps(context, additional_context)

                  results = results(dep_paths, context)

                  action_input
                  |> Ash.Flow.Template.set_dependent_values(%{
                    results: results,
                    elements: Map.get(context, :_ash_engine_elements)
                  })
                  |> Ash.Flow.handle_modifiers()
                end
              )
            end
          )
        )

      %Step{step: %Ash.Flow.Step.Validate{only_keys: keys} = validate, input: input} ->
        %{
          action: action,
          name: name,
          resource: resource,
          input: action_input,
          record: record,
          tenant: tenant,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        } = validate

        %{
          action: action,
          resource: resource,
          action_input: action_input,
          dep_paths: dep_paths,
          tenant: tenant,
          request_deps: request_deps
        } =
          action_request_info(
            all_steps,
            resource,
            action,
            nil,
            action_input,
            input,
            transaction_name,
            tenant,
            [wait_for, halt_if]
          )

        {changeset_deps, get_request} =
          if action.type in [:create, :read] do
            get_request(
              record,
              input,
              all_steps,
              transaction_name,
              name,
              resource,
              additional_context
            )
          else
            {[], nil}
          end

        validate_request =
          Ash.Engine.Request.new(
            path: [name, :validate],
            name: "Fetch record for #{inspect(name)}",
            error_path: List.wrap(name) ++ [:fetch],
            async?: must_be_local?(%{resource: resource}),
            resource: resource,
            authorize?: false,
            data:
              Ash.Engine.Request.resolve(request_deps ++ changeset_deps, fn context ->
                context = Ash.Helpers.deep_merge_maps(context, additional_context)

                results = results(changeset_deps ++ dep_paths, context)

                halt_if(halt_if, halt_reason, name, results, context, fn ->
                  tenant =
                    tenant
                    |> Ash.Flow.Template.set_dependent_values(%{
                      results: results,
                      elements: Map.get(context, :_ash_engine_elements)
                    })
                    |> Ash.Flow.handle_modifiers()

                  input =
                    action_input
                    |> Ash.Flow.Template.set_dependent_values(%{
                      results: results,
                      elements: Map.get(context, :_ash_engine_elements)
                    })
                    |> Ash.Flow.handle_modifiers()

                  result =
                    case action.type do
                      :read ->
                        Ash.Query.for_read(resource, action.name, input,
                          tenant: tenant,
                          actor: context[:actor],
                          authorize?: context[:authorize?],
                          tracer: context[:tracer]
                        )

                      :create ->
                        Ash.Changeset.for_create(resource, action.name, input,
                          tenant: tenant,
                          actor: context[:actor],
                          authorize?: context[:authorize?],
                          tracer: context[:tracer]
                        )

                      type when type in [:update, :destroy] ->
                        record
                        |> Ash.Flow.Template.set_dependent_values(%{
                          results: results,
                          elements: Map.get(context, :_ash_engine_elements)
                        })
                        |> Ash.Flow.handle_modifiers()
                        |> case do
                          %^resource{} = record ->
                            Ash.Changeset.for_action(record, action.name, input,
                              tenant: tenant,
                              actor: context[:actor],
                              authorize?: context[:authorize?],
                              tracer: context[:tracer]
                            )

                          nil ->
                            {:error, "Cannot validate #{action.name} because input is nil"}

                          value ->
                            {:error, "Invalid record #{inspect(value)}"}
                        end
                    end

                  extract_changeset_or_query_errors(result, keys)
                end)
              end)
          )

        if get_request do
          [get_request, validate_request]
        else
          [validate_request]
        end

      %Step{step: %Ash.Flow.Step.Update{} = update, input: input} ->
        %{
          action: action,
          api: api,
          name: name,
          resource: resource,
          input: action_input,
          record: record,
          tenant: tenant,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        } = update

        List.wrap(
          maybe_dynamic(
            [resource, action],
            name,
            [name, :commit],
            input,
            all_steps,
            transaction_name,
            additional_context,
            fn resource, action ->
              %{
                action: action,
                action_input: action_input,
                resource: resource,
                api: api,
                dep_paths: dep_paths,
                tenant: tenant,
                request_deps: request_deps
              } =
                action_request_info(
                  all_steps,
                  resource,
                  action,
                  api,
                  action_input,
                  input,
                  transaction_name,
                  tenant,
                  [wait_for, halt_if]
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
                    tracer: opts[:tracer],
                    changeset_dependencies: [[name, :fetch, :data] | request_deps],
                    skip_on_nil_record?: true,
                    modify_changeset: fn changeset, context ->
                      context = Ash.Helpers.deep_merge_maps(context, additional_context)
                      results = results(dep_paths, context)

                      halt_if(halt_if, halt_reason, name, results, context, fn -> changeset end)
                    end,
                    tenant: fn context ->
                      context = Ash.Helpers.deep_merge_maps(context, additional_context)
                      results = results(dep_paths, context)

                      tenant
                      |> Ash.Flow.Template.set_dependent_values(%{
                        results: results,
                        elements: Map.get(context, :_ash_engine_elements)
                      })
                      |> Ash.Flow.handle_modifiers()
                    end,
                    record: fn context ->
                      Ash.Flow.do_get_in(context, [name, :fetch, :data])
                    end,
                    changeset_input: fn context ->
                      context = Ash.Helpers.deep_merge_maps(context, additional_context)

                      results = results(dep_paths, context)

                      action_input
                      |> Ash.Flow.Template.set_dependent_values(%{
                        results: results,
                        elements: Map.get(context, :_ash_engine_elements)
                      })
                      |> Ash.Flow.handle_modifiers()
                    end
                  )
              ]
            end
          )
        )

      %Step{step: %Ash.Flow.Step.Destroy{} = destroy, input: input} ->
        %{
          action: action,
          api: api,
          name: name,
          resource: resource,
          input: action_input,
          record: record,
          tenant: tenant,
          wait_for: wait_for,
          halt_if: halt_if,
          halt_reason: halt_reason
        } = destroy

        List.wrap(
          maybe_dynamic(
            [resource, action],
            name,
            [name, :commit],
            input,
            all_steps,
            transaction_name,
            additional_context,
            fn resource, action ->
              %{
                action: action,
                action_input: action_input,
                resource: resource,
                api: api,
                dep_paths: dep_paths,
                tenant: tenant,
                request_deps: request_deps
              } =
                action_request_info(
                  all_steps,
                  resource,
                  action,
                  api,
                  action_input,
                  input,
                  transaction_name,
                  tenant,
                  [wait_for, halt_if]
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
                    tracer: opts[:tracer],
                    changeset_dependencies: [[name, :fetch, :data] | request_deps],
                    skip_on_nil_record?: true,
                    record: fn context ->
                      Ash.Flow.do_get_in(context, [name, :fetch, :data])
                    end,
                    modify_changeset: fn changeset, context ->
                      context = Ash.Helpers.deep_merge_maps(context, additional_context)
                      results = results(dep_paths, context)

                      halt_if(halt_if, halt_reason, name, results, context, fn -> changeset end)
                    end,
                    tenant: fn context ->
                      context = Ash.Helpers.deep_merge_maps(context, additional_context)
                      results = results(dep_paths, context)

                      tenant
                      |> Ash.Flow.Template.set_dependent_values(%{
                        results: results,
                        elements: Map.get(context, :_ash_engine_elements)
                      })
                      |> Ash.Flow.handle_modifiers()
                    end,
                    changeset_input: fn context ->
                      context = Ash.Helpers.deep_merge_maps(context, additional_context)

                      results = results(dep_paths, context)

                      action_input
                      |> Ash.Flow.Template.set_dependent_values(%{
                        results: results,
                        elements: Map.get(context, :_ash_engine_elements)
                      })
                      |> Ash.Flow.handle_modifiers()
                    end
                  )
              ]
            end
          )
        )
    end
  end

  defp maybe_dynamic(
         items,
         name,
         request_path,
         input,
         all_steps,
         transaction_name,
         additional_context,
         fun
       ) do
    if Ash.Flow.Template.is_template?(items) do
      {items, deps} =
        Enum.reduce(items, {[], []}, fn item, {items, deps} ->
          {item, new_deps} = Ash.Flow.Template.handle_input_template(item, input)
          {[item | items], deps ++ new_deps}
        end)

      dep_paths = get_dep_paths(all_steps, deps, transaction_name, [])
      request_deps = dependable_request_paths(dep_paths)

      Ash.Engine.Request.new(
        authorize?: false,
        async?: false,
        name: "Run dynamic step #{inspect(name)}",
        path: [:dynamic, request_path],
        error_path: [],
        data:
          Ash.Engine.Request.resolve(request_deps, fn context ->
            context = Ash.Helpers.deep_merge_maps(context, additional_context)

            results = results(dep_paths, context)

            items =
              items
              |> Ash.Flow.Template.set_dependent_values(%{
                results: results,
                elements: Map.get(context, :_ash_engine_elements)
              })
              |> Ash.Flow.handle_modifiers()

            requests =
              fun
              |> apply(Enum.reverse(items))
              |> List.wrap()

            {:ok, nil, %{requests: requests}}
          end)
      )
    else
      apply(fun, items)
    end
  end

  defp extract_changeset_or_query_errors(result, keys) do
    keys =
      Enum.map(keys, fn key ->
        if is_list(key) do
          {:lists.droplast(key), List.last(key)}
        else
          {[], key}
        end
      end)

    result.errors
    |> expand_errors()
    |> Enum.filter(&error_applies?(&1, keys))
  end

  defp error_applies?(error, keys) do
    Enum.any?(keys, fn {path, key} ->
      path_matches?(error.path, path ++ [key]) ||
        (path_matches?(error.path || [], path) && field_matches?(error, key))
    end)
  end

  defp field_matches?(error, key) do
    key in List.wrap(Map.get(error, :fields)) || key == Map.get(error, :field)
  end

  defp path_matches?(nil, []), do: true
  defp path_matches?(path, path), do: true
  defp path_matches?(_, _), do: false

  defp expand_errors(errors) when is_list(errors) do
    Enum.flat_map(errors, &expand_errors/1)
  end

  defp expand_errors(%class_mod{} = error)
       when class_mod in [
              Ash.Error.Forbidden,
              Ash.Error.Framework,
              Ash.Error.Invalid,
              Ash.Error.Unkonwn
            ] do
    Enum.flat_map(error.errors, &expand_errors/1)
  end

  defp expand_errors(other), do: List.wrap(other)

  defp set_index(name, i) do
    List.wrap(i) ++ List.wrap(name)
  end

  defp results(dep_paths, context) do
    Map.new(dep_paths, fn {name, %{fetch_path: fetch_path}} ->
      {name, Ash.Flow.do_get_in(context, fetch_path)}
    end)
  end

  defp map_steps(steps, fun) do
    steps
    |> Enum.map(fn %Step{step: inner_step} = step ->
      %{step | step: fun.(inner_step)}
    end)
    |> Enum.map(fn
      %Step{step: %{steps: inner_steps}} = step ->
        %{step | step: %{step.step | steps: map_steps(inner_steps, fun)}}

      step ->
        step
    end)
  end

  defp map_outer_steps(steps, fun) do
    steps
    |> Enum.map(fn step ->
      fun.(step)
    end)
    |> Enum.map(fn
      %Step{step: %{steps: inner_steps}} = step ->
        %{step | step: %{step.step | steps: map_outer_steps(inner_steps, fun)}}

      step ->
        step
    end)
  end

  defp remap_step_names(steps, fun, step_names \\ nil) do
    step_names = step_names || step_names(steps)

    steps
    |> Enum.map(fn
      %Step{step: %{name: name} = inner_step} = step ->
        %{step | step: %{inner_step | name: fun.(name)}}
        |> remap_step(step_names, fun)
    end)
    |> Enum.map(fn
      %Step{step: %{steps: inner_steps}} = step ->
        %{step | step: %{step.step | steps: remap_step_names(inner_steps, fun, step_names)}}

      other ->
        other
    end)
  end

  defp step_names(steps) do
    Enum.flat_map(steps, fn step ->
      case step.step do
        %{name: name, steps: steps} ->
          [name | step_names(steps)]

        %{name: name} ->
          [name]
      end
    end)
  end

  defp update_step(steps, name, func) do
    Enum.map(steps, fn step ->
      if step.step.name == name do
        func.(step)
      else
        case step do
          %{step: %{steps: steps}} ->
            %{step | step: %{step.step | steps: update_step(steps, name, func)}}

          step ->
            step
        end
      end
    end)
  end

  # output needs a config
  @remapped_step_keys [
    input: [],
    over: [],
    output: [
      raw?: true
    ],
    condition: [],
    record: [],
    tenant: [],
    wait_for: [],
    halt_if: []
  ]

  defp remap_step(step, step_names, fun) do
    Enum.reduce(@remapped_step_keys, step, fn {key, config}, step ->
      remap_key(step, step_names, key, config, fun)
    end)
  end

  defp remap_key(%{step: step} = outer_step, step_names, key, config, fun) do
    case Map.fetch(step, key) do
      {:ok, value} ->
        new_value =
          if config[:raw?] do
            if value do
              fun.(value)
            end
          else
            Ash.Flow.Template.remap_result_references(value, fn name ->
              if name in step_names do
                fun.(name)
              else
                name
              end
            end)
          end

        %{outer_step | step: Map.put(step, key, new_value)}

      :error ->
        outer_step
    end
  end

  defp get_flow_return_value(
         steps,
         name,
         data,
         transaction_name,
         in_transaction? \\ false,
         default \\ nil
       ) do
    case find_step_dep(steps, name, transaction_name) do
      nil ->
        default

      %Ash.Flow.Step.Transaction{} = transaction ->
        data =
          if in_transaction? do
            Ash.Flow.do_get_in(data, [transaction.name])
          else
            Ash.Flow.do_get_in(data, [transaction.name, :data])
          end

        get_flow_return_value(transaction.steps, name, data, transaction.name, true, data)

      step ->
        if in_transaction? do
          Ash.Flow.do_get_in(data, [step.name])
        else
          Ash.Flow.do_get_in(data, result_path(%{step | name: name}))
        end
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
    {record, record_deps} = Ash.Flow.Template.handle_input_template(record, input)

    get_request_dep_paths =
      all_steps
      |> get_dep_paths(record_deps, transaction_name, [])

    get_request_deps = dependable_request_paths(get_request_dep_paths)

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

          results = results(get_request_dep_paths, context)

          record
          |> Ash.Flow.Template.set_dependent_values(%{
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
         api,
         action_input,
         input,
         transaction_name,
         tenant,
         additional
       ) do
    {action_input, deps} = Ash.Flow.Template.handle_input_template(action_input, input)
    {tenant, tenant_deps} = Ash.Flow.Template.handle_input_template(tenant, input)
    {_, additional_deps} = Ash.Flow.Template.handle_input_template(additional, input)
    {resource, resource_deps} = Ash.Flow.Template.handle_input_template(resource, input)
    {api, api_deps} = Ash.Flow.Template.handle_input_template(api, input)
    {action, action_deps} = Ash.Flow.Template.handle_input_template(action, input)

    action =
      Ash.Resource.Info.action(resource, action) ||
        raise "No such action #{action} for #{resource}"

    dep_paths =
      get_dep_paths(
        all_steps,
        deps ++ tenant_deps ++ api_deps ++ resource_deps ++ action_deps,
        transaction_name,
        additional_deps
      )

    request_deps = dependable_request_paths(dep_paths)

    %{
      resource: resource,
      api: api,
      action: action,
      action_input: action_input,
      dep_paths: dep_paths,
      tenant: tenant,
      request_deps: request_deps
    }
  end

  defp dependable_request_paths(dep_paths) do
    for {_, %{request_path: request_path, depend?: true}} <- dep_paths do
      request_path
    end
  end

  defp get_dep_paths(all_steps, deps, transaction_name, wait_for) do
    # We favor the result dependencies over the completion dependencies
    Map.merge(
      dep_paths(wait_for, all_steps, transaction_name, :completion),
      dep_paths(deps, all_steps, transaction_name)
    )
  end

  defp dep_paths(deps, steps, transaction_name, result_type \\ :result) do
    deps
    |> Enum.reduce(%{}, fn
      {:_result, dep}, acc ->
        case find_step_dep(steps, dep, transaction_name, false, true) do
          nil ->
            acc

          {step, :no_depend} ->
            add_dep(step, dep, acc, result_type, false)

          step ->
            add_dep(step, dep, acc, result_type, true)
        end

      _, acc ->
        acc
    end)
  end

  defp add_dep(step, dep, acc, type, depend?) do
    case step do
      %Ash.Flow.Step.Transaction{name: name} ->
        request_path =
          if type == :completion do
            [name, :completion]
          else
            [name, :data]
          end

        Map.put(acc, dep, %{
          fetch_path: [name, :data, dep],
          request_path: request_path,
          depend?: depend?
        })

      step ->
        request_path =
          if type == :completion do
            completion_path(step)
          else
            result_path(step)
          end

        Map.put(acc, dep, %{
          fetch_path: request_path,
          request_path: request_path,
          depend?: depend?
        })
    end
  end

  defp find_step_dep(
         steps,
         dep,
         transaction_name,
         stop_at_transaction_name? \\ false,
         no_depend? \\ false
       ) do
    Enum.find_value(steps, fn
      %Step{step: %Ash.Flow.Step.Transaction{name: ^dep} = step} ->
        if transaction_name && no_depend? do
          {step, :no_depend}
        else
          step
        end

      %Step{step: %Ash.Flow.Step.Transaction{name: ^transaction_name}}
      when stop_at_transaction_name? ->
        nil

      %Step{step: %Ash.Flow.Step.Transaction{name: ^transaction_name, steps: steps}} ->
        find_step_dep(steps, dep, nil, stop_at_transaction_name?, no_depend?)

      %Step{step: %Ash.Flow.Step.Transaction{steps: steps} = step} ->
        case find_step_dep(steps, dep, transaction_name, stop_at_transaction_name?, no_depend?) do
          nil ->
            nil

          inner_step ->
            if transaction_name do
              inner_step
            else
              step
            end
        end

      %Step{step: %{name: ^dep} = step} ->
        if transaction_name && no_depend? do
          {step, :no_depend}
        else
          step
        end

      %Step{step: %{steps: steps}} ->
        find_step_dep(steps, dep, transaction_name, stop_at_transaction_name?, no_depend?)

      _ ->
        nil
    end)
  end

  defp result_path(%Ash.Flow.Step.Read{name: name}) do
    [name, :data, :data]
  end

  defp result_path(%Ash.Flow.Step.Create{name: name}) do
    [name, :commit, :data]
  end

  defp result_path(%Ash.Flow.Step.Destroy{name: name}) do
    [name, :commit, :data]
  end

  defp result_path(%Ash.Flow.Step.Validate{name: name}) do
    [name, :validate, :data]
  end

  defp result_path(%Ash.Flow.Step.Update{name: name}) do
    [name, :commit, :data]
  end

  defp result_path(%Ash.Flow.Step.Transaction{name: name}) do
    [name, :data]
  end

  defp result_path(%Ash.Flow.Step.RunFlow{name: name}) do
    [name, :data]
  end

  defp result_path(%Ash.Flow.Step.Branch{name: name}) do
    [name, :data]
  end

  defp result_path(%Ash.Flow.Step.Map{name: name}) do
    [name, :data]
  end

  defp result_path(%Ash.Flow.Step.Custom{name: name}) do
    [name, :data]
  end

  defp result_path(%Ash.Flow.Step.Debug{name: name}) do
    [name, :data]
  end

  defp completion_path(%Ash.Flow.Step.Read{name: name}) do
    [name, :data, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Create{name: name}) do
    [name, :commit, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Validate{name: name}) do
    [name, :commit, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Update{name: name}) do
    [name, :commit, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Destroy{name: name}) do
    [name, :commit, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Transaction{name: name}) do
    [name, :completion]
  end

  defp completion_path(%Ash.Flow.Step.RunFlow{name: name}) do
    [name, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Branch{name: name}) do
    [name, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Map{name: name}) do
    [name, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Custom{name: name}) do
    [name, :completion]
  end

  defp completion_path(%Ash.Flow.Step.Debug{name: name}) do
    [name, :completion]
  end

  defp data_path(%Ash.Flow.Step.Debug{name: name}) do
    [name, :data]
  end

  defp data_path(%Ash.Flow.Step.Transaction{name: name}) do
    [name, :data]
  end

  defp data_path(%Ash.Flow.Step.Read{name: name}) do
    [name, :data]
  end

  defp data_path(%Ash.Flow.Step.Create{name: name}) do
    [name, :commit]
  end

  defp data_path(%Ash.Flow.Step.Destroy{name: name}) do
    [name, :commit]
  end

  defp data_path(%Ash.Flow.Step.Validate{name: name}) do
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

  defp data_path(%Ash.Flow.Step.Branch{name: name}) do
    [name]
  end

  defp data_path(%Ash.Flow.Step.Map{name: name}) do
    [name]
  end
end
