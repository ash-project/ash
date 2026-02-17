defmodule ETS do
  use ETS.Utils

  @moduledoc """
  ETS, an Elixir wrapper for Erlang's [`:ets`](http://erlang.org/doc/man/ets.html) module.

  See `ETS.Set` for information on creating and managing Sets, and `ETS.Bag` for information on creating and managing Bags.

  See `ETS.KeyValueSet` for an abstraction which provides standard key/value interaction with Sets.

  ## What type of `ETS` table should I use?

  ## Set

  If you need your key column to be unique, then you should use a Set. If you just want a simple key/value store,
  then use an `ETS.KeyValueSet`, but if you want to store full tuple records, use an `ETS.Set`. If you want your
  records ordered by key value, which adds some performance overhead on insertion, set `ordered: true` when creating the Set (defaults to false).

  ## Bag

  If you do not need your key column to be unique, then you should use an `ETS.Bag`, and if you want to prevent exact duplicate
  records from being inserted, which adds some performance overhead on insertion, set duplicate: false when creating the Bag
  (defaults to true).
  """

  @type table_name :: atom()
  @type table_reference :: :ets.tid()
  @type table_identifier :: table_name | table_reference
  @type match_pattern :: :ets.match_pattern()
  @type match_spec :: :ets.match_spec()
  @type comp_match_spec :: :ets.comp_match_spec()
  @type end_of_table :: :"$end_of_table"
  @type continuation ::
          end_of_table
          | {table_reference(), integer(), integer(), comp_match_spec(), list(), integer()}
          | {table_reference(), any(), any(), integer(), comp_match_spec(), list(), integer(),
             integer()}

  @doc """
  Returns list of current :ets tables, each wrapped as either `ETS.Set` or `ETS.Bag`.

  NOTE: `ETS.Bag` is not yet implemented. This list returns only :set and :ordered_set tables, both wrapped as `ETS.Set`.

  ## Examples

      iex> {:ok, all} = ETS.all()
      iex> x = length(all)
      iex> ETS.Set.new!()
      iex> {:ok, all} = ETS.all()
      iex> length(all) == x + 1
      true

  """
  @spec all :: {:ok, [ETS.table_identifier()]} | {:error, any()}
  def all do
    catch_error do
      all =
        :ets.all()
        |> Enum.map(fn tid ->
          tid
          |> :ets.info()
          |> Keyword.get(:type)
          |> case do
            type when type in [:set, :ordered_set] -> ETS.Set.wrap_existing!(tid)
            type when type in [:bag, :duplicate_bag] -> ETS.Bag.wrap_existing!(tid)
          end
        end)

      {:ok, all}
    end
  end

  @doc """
  Same as all/1 but unwraps or raises on :error.

  """
  @spec all! :: [ETS.table_identifier()]
  def all!, do: unwrap_or_raise(all())
end
