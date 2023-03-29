# Read Actions & Data Loading Refactor

Problems:

1. Loading data - lots of expensive comparison operations being performed to check what's been loaded, etc.
  - calculation metadata is recursively populated
  - no simple format for declaring the data that needs to be loaded
2. Read action flow


Idea: use Ash.Flow to implement read (and in future all) actions.
Needs:
  1. First class ability of steps to generate new steps.
  2. (Maybe) ergonomics improvements to some conditional steps, eg: `branch` rather than `if/else`.
  3. Maybe conditional dependencies?

# Actions:
1. Feature flag for switching between `read.ex` and `read_flow.ex`.
2. Iterate until all the tests pass.

# Suspicions:
1. Ash.Flow step generator.
2. Ash.Flow if/else steps.
3. Conditional dependencies?



This is too big to fail, so: let's just do calculations.

New structure for calculations:

Benefits:

simpler to manipulate and check what calculations have been loaded, and comparing what calculations have been loaded no longer *also must* compare their definitions.

Note:
we end up removing `name` and `load` from `Ash.Query.Calculation` (maybe eventually? In Ash 3.0? If we can achieve backwards compatibility then we'd leave them. Maybe won't matter, needs to be thought about)

Filters will be fine without the keys above because they don't need to call them anything to put them into an expression


```elixir
  @type calculation_definition :: %{name: atom(), target: target()}
  @type target :: {:calculations, atom()} | {:top_level, atom()}
  # better names for this key to be workshopped
  calculations_to_load: %{
    score1: %{args: %{}, load_as: :}
    full_name: %{arg1: 10},
    full_name: %{arg1: 11},
    full_name: %{arg1: 12},
    some_random_shit: %{arg1: 10}
  },
  calculation_definitions: %{
    # Supports anonymous calculations & resource calculations
    %{name: :full_name, target: {:top_level, :full_name}} => %Ash.Query.Calculation{},
    %{name: :full_name, target: {:calculations, :full_name_2}} => %Ash.Query.Calculation{}
  }
```

Scratch

```elixir
  %{
    full_name: %Calculation{},
    some_random_shit: %Calculation{name: :some_random_shit}
  }

  %Calculation{name: :full_name, load: , context: %{arg1: 10, actor: ..., ...., ...}}
  

  calculations do
    calculate :full_name, :string, 
      expr(first_name <> ^arg(:separator) <> last_name) do
     argument :separator, :string do
          allow_nil? false
          default " "
        end
    end
  end

  User
  |> Ash.Query.load(full_name: %{separator: "~"})
  |> Ash.Query.calculate(:thing, Ash.Query.Calculation.new(...))

  %Resource{
    thing: thing,
    calculations: %{
      thing2: thing
    }
  }
  %{name: :thing, target: :thing}
  %{name: :thing, target: {:calculations, :thing2}}

  calculations: %{
    full_name: %{args: %{separator: "~"}}
  },
  calculation_definitions: %{
    %{name: :full_name, load: :full_name} => %Ash.Query.Calculation{}
  }

  %Resource{
    name: :value,
    calculations: %{}
  }
  
  Ash.Resource.Info.calculation(query.resource, calc_name,)
  
  some_read_action
  |> read_them()
  |> Enum.map(...)
  |> calculation(....)

  
  query
  |> Ash.Query.load_calculation_as(:score, :score_1, %{arg: 1})
  |> Ash.Query.load_calculation_as(:score, :score_1, %{arg: 1})
```

```elixir
# What calculate looks like now, context jumbled in with context
def calculate(records, opts, %{arg1: arg1, actor: actor}) do

end

# What it should look like < 3.0 >
def calculate(records, opts, args, %CalculationContext{actor: actor, tenant: tenant, authorize?: authorize?} = context) do

end
```