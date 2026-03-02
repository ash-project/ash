# SPDX-FileCopyrightText: 2024 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Notifier.NotifierDependencies do
  @moduledoc false

  # An internal calculation used to load notifier dependencies through the
  # calculation dependency resolver. Each notifier that implements `load/2`
  # gets its own instance of this calculation (keyed by notifier module).
  # The dep resolver deduplicates identical underlying loads across notifiers.
  # The result is a map of the loaded field values for that notifier.
  #
  # The load statement can contain plain atoms, keyword pairs, or
  # %Ash.Query.Calculation{} structs (e.g. from a PubSub notifier building
  # one inner NotifierDependencies per publication).

  use Ash.Resource.Calculation

  @impl true
  def load(_query, opts, _context) do
    opts[:statement] || []
  end

  @impl true
  def calculate(records, opts, _context) do
    statement = List.wrap(opts[:statement] || [])

    {:ok,
     Enum.map(records, fn record ->
       struct_map = Map.from_struct(record)
       calculations = Map.get(record, :calculations) || %{}

       Enum.reduce(statement, %{}, fn entry, acc ->
         {key, value} =
           case entry do
             %Ash.Query.Calculation{name: name, load: load} when not is_nil(load) ->
               # result was placed directly on the struct under the `load` key
               {name, Map.get(struct_map, load)}

             %Ash.Query.Calculation{name: name} ->
               # result is in record.calculations[name]
               {name, Map.get(calculations, name)}

             {field, _opts} when is_atom(field) ->
               value =
                 if Map.has_key?(struct_map, field) do
                   Map.get(struct_map, field)
                 else
                   Map.get(calculations, field)
                 end

               {field, value}

             field when is_atom(field) ->
               value =
                 if Map.has_key?(struct_map, field) do
                   Map.get(struct_map, field)
                 else
                   Map.get(calculations, field)
                 end

               {field, value}
           end

         Map.put(acc, key, value)
       end)
     end)}
  end
end
