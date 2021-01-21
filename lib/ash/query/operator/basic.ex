defmodule Ash.Query.Operator.Basic do
  @operators [
    plus: [
      symbol: :+
    ],
    times: [
      symbol: :*
    ],
    minus: [
      symbol: :-
    ],
    div: [
      symbol: :/
    ]
  ]

  Module.register_attribute(__MODULE__, :operator_modules, accumulate: true)

  for {name, opts} <- @operators do
    mod = Module.concat([__MODULE__, String.capitalize(to_string(name))])
    @operator_modules mod

    Module.create(
      mod,
      quote do
        @moduledoc """
        left #{unquote(opts[:symbol])} right
        """

        use Ash.Query.Operator,
          operator: unquote(opts[:symbol]),
          name: unquote(name),
          predicate?: false,
          types: [:same, :any]

        def evaluate(%{left: left, right: right}) do
          {:known, apply(Kernel, unquote(opts[:symbol]), [left, right])}
        end
      end,
      Macro.Env.location(__ENV__)
    )
  end

  def operator_modules do
    @operator_modules
  end
end
