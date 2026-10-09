# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.NestedRelationshipCalcMergeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias __MODULE__.{Child, Parent, Pause, Rule}

  defmodule Constant do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def calculate(records, opts, _context), do: Enum.map(records, fn _ -> opts[:value] end)
  end

  defmodule Window do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, %{arguments: %{with_pauses?: true}}),
      do: [rule: [parent: [:timezone, :pauses]]]

    def load(_query, _opts, _context), do: [rule: [parent: [:timezone]]]

    @impl true
    def calculate(children, _opts, %{arguments: %{with_pauses?: with_pauses?}}) do
      Enum.map(children, fn child ->
        pauses = if with_pauses?, do: Enum.map(child.rule.parent.pauses, & &1.id)
        %{timezone: child.rule.parent.timezone, pauses: pauses}
      end)
    end
  end

  defmodule WithWindow do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, opts, _context), do: [window: [with_pauses?: opts[:with_pauses?]]]

    @impl true
    def calculate(children, _opts, _context), do: Enum.map(children, & &1.window)
  end

  defmodule PauseIds do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, _opts, _context), do: [rule: [parent: [:pauses]]]

    @impl true
    def calculate(children, _opts, _context) do
      Enum.map(children, fn child -> Enum.map(child.rule.parent.pauses, & &1.id) end)
    end
  end

  defmodule Forward do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, opts, _context), do: [opts[:calc]]

    @impl true
    def calculate(records, opts, _context), do: Enum.map(records, &Map.get(&1, opts[:calc]))
  end

  defmodule LoadChildren do
    @moduledoc false
    use Ash.Resource.Calculation

    @impl true
    def load(_query, opts, _context), do: [children: [opts[:calc]]]

    @impl true
    def calculate(parents, opts, _context) do
      Enum.map(parents, fn parent -> Enum.map(parent.children, &Map.get(&1, opts[:calc])) end)
    end
  end

  defmodule Pause do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :parent, Parent, public?: true, attribute_writable?: true
    end
  end

  defmodule Rule do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :parent, Parent, public?: true, attribute_writable?: true
    end
  end

  defmodule Child do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :parent, Parent, public?: true, attribute_writable?: true
      belongs_to :rule, Rule, public?: true, attribute_writable?: true
    end

    calculations do
      calculate :one, :integer, {Constant, value: 1}
      calculate :two, :integer, {Constant, value: 2}

      calculate :window, :map, Window do
        argument :with_pauses?, :boolean, allow_nil?: false
      end

      calculate :timetable, :map, {WithWindow, with_pauses?: false}
      calculate :todos, :map, {WithWindow, with_pauses?: true}
      calculate :forwarded_todos, :map, {Forward, calc: :todos}
      calculate :pause_ids, {:array, :uuid}, PauseIds
    end
  end

  defmodule Parent do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :timezone, :string, public?: true
    end

    relationships do
      has_many :children, Child, public?: true
      has_many :pauses, Pause, public?: true
    end

    calculations do
      # each of these loads the calculation of the same name on `children`
      calculate :one, {:array, :integer}, {LoadChildren, calc: :one}
      calculate :two, {:array, :integer}, {LoadChildren, calc: :two}
      calculate :timetable, {:array, :map}, {LoadChildren, calc: :timetable}
      calculate :todos, {:array, :map}, {LoadChildren, calc: :todos}

      calculate :children_pause_ids,
                {:array, {:array, :uuid}},
                {LoadChildren, calc: :pause_ids}
    end
  end

  setup do
    parent = Ash.Seed.seed!(Parent, %{timezone: "Europe/Berlin"})
    rule = Ash.Seed.seed!(Rule, %{parent_id: parent.id})
    Ash.Seed.seed!(Child, %{parent_id: parent.id, rule_id: rule.id})
    pause = Ash.Seed.seed!(Pause, %{parent_id: parent.id})

    %{parent: parent, pause: pause}
  end

  for authorize? <- [true, false] do
    test "calculations load same-named related calculations through one relationship, authorize?: #{authorize?}",
         %{parent: parent} do
      assert %{one: [1], two: [2]} =
               Ash.get!(Parent, parent.id, load: [:one, :two], authorize?: unquote(authorize?))

      assert %{one: [1], two: [2]} =
               Ash.load!(parent, [:one, :two], authorize?: unquote(authorize?))
    end
  end

  test "a related calculation merged into an already loaded relationship gets its nested loads",
       %{parent: parent, pause: pause} do
    loaded =
      Ash.get!(Parent, parent.id,
        load: [:children_pause_ids, children: [:rule]],
        authorize?: false
      )

    assert loaded.children_pause_ids == [[pause.id]]
    assert [%{rule: %Rule{}}] = loaded.children
  end

  test "a dependency renamed to avoid a conflicting calculation is not replaced by an alias to itself",
       %{pause: pause} do
    # `todos` is a dependency of `forwarded_todos`, and its own dependency
    # `window(with_pauses?: true)` is renamed because `window` is loaded with other arguments
    child = Ash.read_one!(Child, load: [:forwarded_todos, window: [with_pauses?: false]])

    assert child.window == %{timezone: "Europe/Berlin", pauses: nil}
    assert child.forwarded_todos == %{timezone: "Europe/Berlin", pauses: [pause.id]}
  end

  test "related calculations load a shared calculation with different arguments",
       %{parent: parent, pause: pause} do
    loaded = Ash.get!(Parent, parent.id, load: [:timetable, :todos], authorize?: false)

    assert loaded.timetable == [%{timezone: "Europe/Berlin", pauses: nil}]
    assert loaded.todos == [%{timezone: "Europe/Berlin", pauses: [pause.id]}]
  end
end
