defmodule Ash.Domain.Functions do
  @moduledoc false

  @functions [
    stream!: 1,
    count: 1,
    count!: 1,
    first: 2,
    first!: 2,
    sum: 2,
    sum!: 2,
    min: 2,
    min!: 2,
    max: 2,
    max!: 2,
    avg: 2,
    avg!: 2,
    exists: 1,
    exists?: 1,
    list: 2,
    list!: 2,
    aggregate: 2,
    aggregate!: 2,
    can?: 2,
    can: 2,
    calculate: 2,
    calculate!: 2,
    run_action: 1,
    run_action!: 1,
    get: 2,
    get!: 2,
    read_one: 1,
    read_one!: 1,
    read: 1,
    read!: 1,
    page: 2,
    load: 2,
    load!: 2,
    create: 1,
    create!: 1,
    bulk_create: 3,
    bulk_create!: 3,
    bulk_update: 3,
    bulk_update!: 3,
    bulk_destroy: 3,
    bulk_destroy!: 3,
    update: 1,
    update!: 1,
    destroy: 1,
    destroy!: 1,
    reload: 1,
    reload!: 1
  ]

  @no_opts_functions [
    :page
  ]

  def functions, do: @functions
  def no_opts_functions, do: @no_opts_functions
end
