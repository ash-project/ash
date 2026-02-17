# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Mix.Tasks.Reactor.Run do
  @shortdoc "Run a Reactor from the command line"
  @moduledoc """
  #{@shortdoc}

  The `reactor.run` mix task allows you to run an arbitrary reactor on the commandline and return it's result to STDOUT.

  ## Example

  ```
  $ mix reactor.run Example.HelloWorldReactor --input-whom="Marty"
  "Hello, Marty"
  ```

  ## Usage

  - `--input` specify that the reactor input should retrieved from STDIN either as `yaml` or `json`. Must be present unless all inputs are coverted by `--input-<name>` inputs. 
  - `--input-<name>` specify a specific input on the command-line. No casting is performed on the input and it is assumed to be a string.
  - `--format` specify the output format for the result, which is send to STDOUT. Either `yaml` or `json`.  Defaults to `yaml`.
  """
  use Mix.Task
  alias Spark.Dsl.Extension

  @initial_switch_schema [
    input: :string,
    format: :string
  ]

  @initial_option_schema [
    input: [
      type: {:in, ["json", "yaml", "JSON", "YAML"]},
      required: false,
      default: "yaml"
    ],
    format: [
      type: {:in, ["json", "yaml", "JSON", "YAML"]},
      required: false,
      default: "yaml"
    ],
    inputs: [
      type: :keyword_list,
      required: false,
      default: [],
      keys: []
    ]
  ]

  @doc false
  @impl true
  def run([]), do: Mix.Task.run("help", ["reactor.run"])

  def run([reactor | args]) do
    with {:ok, reactor} <- validate_reactor(reactor),
         {:ok, otp_app} <- get_otp_app(reactor),
         :ok <- start_app(otp_app),
         {:ok, switches} <- parse_switches(reactor, args),
         {:ok, options} <- validate_options(reactor, switches),
         {:ok, inputs} <- validate_inputs(reactor, options),
         {:ok, result} <- Reactor.run(reactor, inputs, %{mix_reactor_run?: true}),
         {:ok, result} <- serialise_result(result, String.downcase(options[:format])) do
      Mix.shell().info(result)

      :ok
    else
      {:error, exception} when is_exception(exception) ->
        message = Exception.message(exception)
        Mix.shell().error(message)
        System.stop(1)

      {:error, exception} when is_binary(exception) ->
        if String.printable?(exception) do
          Mix.shell().error(exception)
        else
          Mix.shell().error("An error occurred: `#{inspect(exception)}`")
        end

        System.stop(1)

      {:error, exception} ->
        Mix.shell().error("An error occurred: `#{inspect(exception)}`")
        System.stop(1)
    end
  end

  defp serialise_result(value, "yaml") do
    with {:ok, doc} <- Ymlr.document(value) do
      {:ok, String.trim_leading(doc, "---\n")}
    end
  end

  defp serialise_result(value, "json") do
    Jason.encode(value, pretty: true)
  end

  defp validate_reactor(reactor) do
    with {:ok, reactor} <- try_load_module(reactor),
         :ok <- reactor?(reactor) do
      {:ok, reactor}
    end
  end

  defp try_load_module(module) do
    module = Module.concat([module])

    case Code.ensure_loaded(module) do
      {:module, module} ->
        {:ok, module}

      {:error, reason} ->
        {:error, "Unable to load Reactor `#{inspect(module)}`: `#{inspect(reason)}`"}
    end
  end

  defp reactor?(module) do
    if function_exported?(module, :spark_is, 0) && module.spark_is() == Reactor do
      :ok
    else
      {:error, "Module `#{inspect(module)}` is not a Reactor module"}
    end
  end

  defp get_otp_app(reactor) do
    case Extension.get_persisted(reactor, :otp_app) do
      nil ->
        {:error,
         "Reactor has no `otp_app` specified. Please add the `otp_app` option to your `use Reactor` statement"}

      app when is_atom(app) ->
        {:ok, app}

      _other ->
        {:error, "Reactor has an invalid `otp_app` specified."}
    end
  end

  defp start_app(otp_app) do
    with {:ok, _} <- Application.ensure_all_started(otp_app) do
      :ok
    end
  end

  defp parse_switches(reactor, args) do
    schema =
      reactor.reactor()
      |> Map.get(:inputs, [])
      |> Enum.reduce(@initial_switch_schema, fn input_name, schema ->
        switch_name = String.to_atom("input_#{input_name}")
        Keyword.put(schema, switch_name, :string)
      end)
      |> then(&[strict: &1])

    case OptionParser.parse(args, schema) do
      {switches, [], []} ->
        {:ok, switches}

      {_, _, errors} ->
        errors =
          Enum.map_join(errors, "\n", fn
            {switch, nil} -> "  - `#{switch}`"
            {switch, value} -> "  - `#{switch}`: `#{value}`"
          end)

        {:error,
         """
         The following arguments we're unable to be parsed:

         #{errors}
         """}
    end
  end

  defp validate_options(reactor, opts) do
    reactor_inputs =
      reactor.reactor()
      |> Map.get(:inputs, [])

    input_schema =
      reactor_inputs
      |> Enum.map(&{&1, [type: :string, required: false]})

    schema = put_in(@initial_option_schema, [:inputs, :keys], input_schema)

    {inputs, opts} =
      Enum.reduce(reactor_inputs, {[], opts}, fn input_name, {inputs, opts} ->
        switch_name = String.to_atom("input_#{input_name}")

        case Keyword.pop(opts, switch_name) do
          {nil, opts} -> {inputs, opts}
          {value, opts} -> {Keyword.put(inputs, input_name, value), opts}
        end
      end)

    opts = Keyword.put(opts, :inputs, inputs)

    Spark.Options.validate(opts, schema)
  end

  defp validate_inputs(reactor, opts) do
    reactor_inputs =
      reactor.reactor()
      |> Map.get(:inputs, [])
      |> MapSet.new()

    provided_inputs = Keyword.get(opts, :inputs, [])

    provided_input_names =
      provided_inputs
      |> Keyword.keys()
      |> MapSet.new()

    if MapSet.equal?(reactor_inputs, provided_input_names) do
      {:ok, Map.new(provided_inputs)}
    else
      format = opts[:input] |> String.upcase()
      read_inputs(format, MapSet.difference(reactor_inputs, provided_input_names))
    end
  end

  defp read_inputs(format, remaining_inputs) do
    prompt_for_inputs(format, remaining_inputs)

    with {:ok, input} <- read_stdin() do
      parse_input(input, remaining_inputs, format)
    end
  end

  defp parse_input(input, remaining_inputs, "YAML") do
    case YamlElixir.read_from_string(input) do
      {:ok, map} when is_map(map) ->
        validate_parsed_input(map, remaining_inputs)

      {:ok, _other} ->
        {:error, "YAML input must be a map"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_input(input, remaining_inputs, "JSON") do
    case Jason.decode(input) do
      {:ok, map} when is_map(map) ->
        validate_parsed_input(map, remaining_inputs)

      {:ok, _other} ->
        {:error, "JSON input must be a map"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp validate_parsed_input(parsed_input, expected_inputs) do
    expected_inputs = Map.new(expected_inputs, &{to_string(&1), &1})

    expected_input_names = expected_inputs |> Map.keys() |> MapSet.new()
    received_input_names = parsed_input |> Map.keys() |> MapSet.new()

    if MapSet.equal?(expected_input_names, received_input_names) do
      {:ok,
       Map.new(parsed_input, fn {key, value} -> {Map.fetch!(expected_inputs, key), value} end)}
    else
      unexpected_inputs = MapSet.difference(received_input_names, expected_input_names)
      missing_inputs = MapSet.difference(expected_input_names, received_input_names)

      cond do
        Enum.any?(unexpected_inputs) && Enum.any?(missing_inputs) ->
          {:error,
           """
           # Error validating input values.

           Received the following unexpected inputs:
           #{Enum.map_join(unexpected_inputs, "\n", &"  - `#{&1}`")}

           The following inputs are missing:
           #{Enum.map_join(missing_inputs, "\n", &"  - `#{&1}`")}
           """}

        Enum.any?(unexpected_inputs) ->
          {:error,
           """
           # Error validating input values.

           Received the following unexpected inputs:
           #{Enum.map_join(unexpected_inputs, "\n", &"  - `#{&1}`")}
           """}

        Enum.any?(missing_inputs) ->
          {:error,
           """
           # Error validating input values.

           The following inputs are missing:
           #{Enum.map_join(missing_inputs, "\n", &"  - `#{&1}`")}
           """}
      end
    end
  end

  defp read_stdin do
    case IO.read(:eof) do
      {:error, reason} -> {:error, "Unable to read input from STDIN: #{inspect(reason)}"}
      :eof -> {:error, "No input received on STDIN"}
      input -> {:ok, input}
    end
  end

  defp prompt_for_inputs(format, remaining_inputs) do
    remaining_inputs = describe_inputs(remaining_inputs)

    Mix.shell().info(
      "Please provide input #{remaining_inputs} in #{format} format (press ^D to finish):"
    )
  end

  defp describe_inputs(inputs) do
    syntax_colors = IO.ANSI.syntax_colors()

    inputs
    |> Enum.sort()
    |> Enum.map(&"`#{inspect(&1, syntax_colors: syntax_colors)}`")
    |> case do
      [input] ->
        input

      inputs ->
        [last | rest] = Enum.reverse(inputs)

        head =
          rest
          |> Enum.reverse()
          |> Enum.join(", ")

        head <> " and #{last}"
    end
  end
end
