# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Map do
  use Reactor.Step
  require Reactor.Argument
  require Iter
  alias Reactor.{Argument, Builder, Step, Template}
  alias Spark.Options
  import Reactor.Utils

  @behaviour Reactor.Mermaid

  @option_schema [
    state: [
      type: {:in, [:init, :iterating]},
      required: true,
      doc: """
      The current execution state of the map.  This is required because it's recursive.
      """
    ],
    batch_size: [
      type: :pos_integer,
      required: false,
      default: 100,
      doc: """
      The number of elements to consume off the source when emitting steps.
      """
    ],
    steps: [
      type: {:list, {:struct, Step}},
      required: true,
      doc: """
      The steps to use when mapping source elements.
      """
    ],
    return: [
      type: :atom,
      required: true,
      doc: """
      The name of the step whose result will be used as the new value for each element.
      """
    ],
    strict_ordering?: [
      type: :boolean,
      required: false,
      default: true,
      doc: """
      Whether the mapped values must be returned in the same order that they were provided.
      """
    ],
    allow_async?: [
      type: :boolean,
      required: false,
      default: true,
      doc: """
      Whether the emitted steps should be allowed to run asynchronously.
      """
    ],
    descendant_step_names: [
      type: {:struct, MapSet},
      required: false,
      doc: """
      The cached names of all descendant steps to aid rewriting. You don't need to provide this value - it is calculated by the init pass.
      """
    ],
    extra_arguments: [
      type: {:list, {:struct, Argument}},
      required: false,
      doc: """
      Extra arguments to be passed by to every nested step.
      """
    ]
  ]

  @moduledoc """
  Given an iterable input run the provided steps for each element and collect
  the results into a new value.

  > #### A note on ordering {: .tip}
  >
  > If your application doesn't need the results back in the same order that
  > they were provided then setting `strict_ordering?` to `false` will increase
  > performance - especially on large input sets.

  ## Options

  #{Options.docs(@option_schema)}
  """

  @doc false
  @impl true
  def run(arguments, context, options) do
    with {:ok, options} <- Options.validate(options, @option_schema) do
      case options[:state] do
        :init -> do_init(arguments.source, arguments, options, context.current_step, context)
        :iterating -> do_iterate(arguments, options, context.current_step)
      end
    end
  end

  @doc false
  @impl true
  def nested_steps(options) do
    Keyword.get(options, :steps, [])
  end

  @doc false
  @impl true
  def to_mermaid(step, options), do: __MODULE__.Mermaid.to_mermaid(step, options)

  defp do_init(source, arguments, options, map_step, context) when Iter.is_iter(source) do
    source =
      source
      |> Iter.with_index()

    # Collect regular extra arguments
    regular_extra_arguments =
      arguments
      |> Map.drop([:source, :result])
      |> Enum.map(fn {name, value} ->
        Argument.from_value(name, value)
      end)

    # Collect nested dependency arguments
    nested_dep_arguments = build_nested_dependency_arguments(context)

    # Merge both sets of extra arguments
    extra_arguments = regular_extra_arguments ++ nested_dep_arguments

    options =
      options
      |> Keyword.put_new_lazy(:descendant_step_names, fn ->
        collect_all_step_names(options[:steps])
      end)
      |> Keyword.put(:state, :iterating)
      |> Keyword.put(:extra_arguments, extra_arguments)

    emit_batch(source, options, map_step, [])
  end

  defp do_init(source, arguments, options, map_step, context) do
    source
    |> Iter.from()
    |> do_init(arguments, options, map_step, context)
  end

  defp build_nested_dependency_arguments(context) do
    nested_deps = Map.get(context, :nested_dependencies, %{})

    # Flatten all nested dependencies into arguments
    nested_deps
    |> Enum.flat_map(fn {_nested_step, args} ->
      Enum.map(args, fn {arg_name, value} ->
        Argument.from_value(arg_name, value)
      end)
    end)
    |> Enum.uniq_by(& &1.name)
  end

  defp do_iterate(arguments, options, map_step) do
    {source, arguments} = Map.pop!(arguments, :source)
    {result, arguments} = Map.pop!(arguments, :result)

    map_step_name = map_step.name

    result =
      Enum.reduce(arguments, result, fn {{__MODULE__, ^map_step_name, :element, index}, value},
                                        result ->
        [{index, value} | result]
      end)

    emit_batch(source, options, map_step, result)
  end

  defp collect_all_step_names(steps, into \\ MapSet.new())
  defp collect_all_step_names([], into), do: into

  defp collect_all_step_names([%{steps: [_ | _] = child_steps} = step | steps], into) do
    into = collect_all_step_names(child_steps, MapSet.put(into, step.name))
    collect_all_step_names(steps, into)
  end

  defp collect_all_step_names([step | steps], into),
    do: collect_all_step_names(steps, MapSet.put(into, step.name))

  defp emit_batch(source, options, map_step, result) do
    with {:done, batch} <- Iter.take_chunk(source, options[:batch_size]),
         {:done, []} <- {:done, Iter.to_list(batch)} do
      finalise_result(result, options)
    else
      {:ok, batch, remainder} -> do_emit_batch(batch, remainder, options, map_step, result)
      {:done, batch} -> do_emit_batch(batch, Iter.empty(), options, map_step, result)
    end
  end

  defp do_emit_batch(batch, remainder, options, map_step, result) do
    with {:ok, arguments} <- arguments_for_batch(batch, options, map_step),
         {:ok, recursive_step} <-
           Builder.new_step(
             map_step.name,
             {__MODULE__, options},
             Enum.concat(arguments, [
               Argument.from_value(:source, remainder),
               Argument.from_result(:result, map_step.name)
             ])
           ),
         {:ok, steps} <- steps_for_batch(batch, options, map_step) do
      steps = Enum.concat(steps, [recursive_step])

      {:ok, result, steps}
    end
  end

  defp finalise_result(result, options) do
    if options[:strict_ordering?] do
      result =
        result
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.map(&elem(&1, 1))

      {:ok, result}
    else
      {:ok, Map.values(result)}
    end
  end

  # generate a whole heap of arguments for the recursive step so that it can
  # collect up the whole batch.
  defp arguments_for_batch(batch, options, map_step) do
    arguments =
      Enum.map(batch, fn {_element, index} ->
        %Argument{
          name: {__MODULE__, map_step.name, :element, index},
          source: %Template.Result{name: {__MODULE__, map_step.name, options[:return], index}}
        }
      end)

    {:ok, arguments}
  end

  defp steps_for_batch(batch, options, map_step) do
    steps = options[:steps]
    descendant_step_names = options[:descendant_step_names]
    extra_arguments = options[:extra_arguments]

    reduce_while_ok(batch, [], fn {element, index}, result ->
      case rewrite_steps_for_element(
             {element, index},
             steps,
             descendant_step_names,
             map_step,
             extra_arguments,
             options[:allow_async?]
           ) do
        {:ok, steps} -> reduce_while_ok(steps, result, &{:ok, [&1 | &2]})
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  defp rewrite_steps_for_element(
         {element, index},
         steps,
         descendant_step_names,
         map_step,
         extra_arguments,
         allow_async?
       ) do
    map_while_ok(
      steps,
      &rewrite_step_for_element(
        &1,
        {element, index},
        descendant_step_names,
        map_step,
        extra_arguments,
        allow_async?
      )
    )
  end

  defp rewrite_step_for_element(
         step,
         {element, index},
         descendant_step_names,
         map_step,
         extra_arguments,
         allow_async?
       ) do
    with {:ok, step} <-
           rewrite_arguments(
             step,
             {element, index},
             descendant_step_names,
             map_step
           ),
         {:ok, step} <-
           rewrite_nested_steps_for_element(
             step,
             {element, index},
             descendant_step_names,
             map_step,
             extra_arguments,
             allow_async?
           ) do
      {:ok,
       %{
         step
         | arguments: Enum.concat(step.arguments, extra_arguments),
           name: {__MODULE__, map_step.name, step.name, index},
           ref: {__MODULE__, map_step.name, step.ref, index},
           async?: allow_async?
       }}
    end
  end

  defp rewrite_arguments(step, {element, index}, descendant_step_names, map_step) do
    map_while_ok(step.arguments, fn
      argument when Argument.is_from_element(argument) ->
        # Check if this element reference is for the current map or should be inherited
        if argument.source.name == map_step.name do
          # This is for the current map - replace with the element value
          argument =
            argument.name
            |> Argument.from_value(element)
            |> Argument.sub_path(argument.source.sub_path)

          {:ok, argument}
        else
          # This is for a parent/outer map - it should have been provided as an extra_argument
          # Keep it as-is, it will be resolved from extra_arguments
          {:ok, argument}
        end

      argument when Argument.is_from_result(argument) ->
        if MapSet.member?(descendant_step_names, argument.source.name) do
          argument = %{
            argument
            | source: %{
                argument.source
                | name: {__MODULE__, map_step.name, argument.source.name, index}
              }
          }

          {:ok, argument}
        else
          {:ok, argument}
        end

      argument ->
        {:ok, argument}
    end)
    |> and_then(&{:ok, %{step | arguments: &1}})
  end

  defp rewrite_nested_steps_for_element(
         %{steps: [_ | _] = steps} = step,
         {element, index},
         descendant_step_names,
         map_step,
         extra_arguments,
         allow_async?
       ) do
    with {:ok, steps} <-
           rewrite_steps_for_element(
             {element, index},
             steps,
             descendant_step_names,
             map_step,
             extra_arguments,
             allow_async?
           ) do
      {:ok, %{step | steps: steps}}
    end
  end

  defp rewrite_nested_steps_for_element(
         step,
         _element_index,
         _descendant_step_names,
         _map_step,
         _extra_arguments,
         _allow_async?
       ),
       do: {:ok, step}
end
