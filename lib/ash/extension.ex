defmodule Ash.Extension do
  @moduledoc """
  A behavior of additional callbacks that extensions can implement, specific to Ash.

  It is not necessary to adopt this behavior, but it is recommended to do so if you want to define these
  functions on your extension. These functions are invoked when their relevant Mix task is run.
  """

  @type argv :: [String.t()]

  @type igniter :: Igniter.t()

  @callback migrate(argv) :: term
  @callback reset(argv) :: term
  @callback rollback(argv) :: term
  @callback setup(argv) :: term
  @callback tear_down(argv) :: term
  @callback codegen(argv) :: term

  @callback install(
              igniter,
              module :: module(),
              type :: Ash.Resource.t() | Ash.Domain.t(),
              location :: String.t(),
              argv
            ) :: igniter

  @optional_callbacks [
    migrate: 1,
    reset: 1,
    rollback: 1,
    setup: 1,
    tear_down: 1,
    codegen: 1,
    install: 5
  ]
end
