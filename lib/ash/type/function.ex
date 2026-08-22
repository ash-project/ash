# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Function do
  @constraints [
    mfa: [
      type: :boolean,
      default: false,
      doc: """
      Restricts the value to external captures (`&Module.function/arity`), refusing \
      anonymous functions and closures. Only external captures can be stored: a closure \
      captures its environment, which cannot be rehydrated in another VM.
      """
    ],
    arity: [
      type: :pos_integer,
      doc: "Enforces a specific arity on the provided function"
    ]
  ]
  @moduledoc """
  Represents a function.

  By default a function is dumped to a native format with
  `:erlang.term_to_binary(term, [:safe])`, and this is *NOT SAFE* to use with
  external input (see https://erlang.org/doc/man/erlang.html#binary_to_term-2).

  With the `mfa` constraint the value is restricted to external captures
  (`&Module.function/arity`) and stored as a portable capture string such as
  `"&Elixir.MyApp.callback/2"`, late-bound back to the function on read. This
  decouples the stored reference from the compiled implementation, so it survives
  across VMs and releases where a serialized closure would not.

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """

  use Ash.Type
  import Ash.Gettext

  @impl true
  def storage_type(constraints) do
    if constraints[:mfa] do
      :string
    else
      :binary
    end
  end

  @impl true
  def constraints, do: @constraints

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(term, constraints) do
    cond do
      constraints[:mfa] && not external?(term) ->
        {:error,
         message:
           error_message(
             "anonymous functions and closures cannot be stored; use a &Module.function/arity capture"
           )}

      constraints[:arity] && not is_function(term, constraints[:arity]) ->
        {:error,
         message: error_message("Expected a function of arity %{arity}"),
         arity: constraints[:arity]}

      true ->
        {:ok, term}
    end
  end

  defp external?(term) when is_function(term) do
    Function.info(term, :type) == {:type, :external}
  end

  defp external?(_), do: false

  @impl true
  def matches_type?(v, _constraints) do
    is_function(v)
  end

  @impl true
  def cast_input(value, _) when is_function(value) do
    {:ok, value}
  end

  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  # A stored value is read by shape, not by constraint: a capture string is
  # parsed and late-bound, anything else is loaded as a legacy `term_to_binary`
  # blob. This is what makes adding `mfa: true` to an attribute with existing
  # rows safe — old binary rows still load, new rows read as capture strings.
  def cast_stored("&" <> _ = value, _constraints) do
    parse_mfa_string(value)
  end

  # sobelow_skip ["Misc.BinToTerm"]
  def cast_stored(value, _constraints) do
    case Ecto.Type.load(:binary, value) do
      {:ok, val} ->
        case :erlang.binary_to_term(val, [:safe]) do
          function when is_function(function) ->
            {:ok, function}

          _ ->
            :error
        end

      other ->
        other
    end
  rescue
    _ ->
      :error
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, constraints) do
    if constraints[:mfa] do
      case mfa_string(value) do
        {:ok, string} -> Ecto.Type.dump(:string, string)
        :error -> :error
      end
    else
      Ecto.Type.dump(:binary, :erlang.term_to_binary(value))
    end
  end

  defp mfa_string(fun) when is_function(fun) do
    case Function.info(fun, :type) do
      {:type, :external} ->
        info = Function.info(fun)

        {:ok,
         "&" <>
           Atom.to_string(info[:module]) <>
           "." <> Atom.to_string(info[:name]) <> "/" <> Integer.to_string(info[:arity])}

      _ ->
        :error
    end
  end

  defp mfa_string(_), do: :error

  defp parse_mfa_string("&" <> rest) do
    with [mod_fun, arity_str] <- String.split(rest, "/", parts: 2),
         {arity, ""} <- Integer.parse(arity_str),
         {:ok, module, name} <- split_module_function(mod_fun),
         fun when is_function(fun) <- Function.capture(module, name, arity) do
      {:ok, fun}
    else
      _ -> :error
    end
  rescue
    # `String.to_existing_atom/1` and `Function.capture/3` raise for an unknown
    # module, function or arity — an unresolvable capture string is not a match.
    _ -> :error
  end

  defp split_module_function(mod_fun) do
    case String.split(mod_fun, ".") do
      [_only] ->
        :error

      parts ->
        {module_parts, [function]} = Enum.split(parts, -1)

        {:ok, String.to_existing_atom(Enum.join(module_parts, ".")),
         String.to_existing_atom(function)}
    end
  end
end
