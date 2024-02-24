# Constraints

Constraints are a way of validating an input type. This validation can be used in both attributes and arguments. The kinds of constraints you can apply depends on the type the data. You can find all types in `Ash.Type` . Each type has its own page on which the available constraints are listed. For example in `Ash.Type.String` you can find 5 constraints:

- `:max_length`
- `:min_length`
- `:match`
- `:trim?`
- `:allow_empty?`

You can also discover these constraints from iex:

```bash
$ iex -S mix
iex(1)> Ash.Type.String.constraints
[
  max_length: [
    type: :non_neg_integer,
    doc: "Enforces a maximum length on the value"
  ],
  min_length: [
    type: :non_neg_integer,
    doc: "Enforces a minimum length on the value"
  ],
  match: [
    type: {:custom, Ash.Type.String, :match, []},
    doc: "Enforces that the string matches a passed in regex"
  ],
  trim?: [type: :boolean, doc: "Trims the value.", default: true],
  allow_empty?: [
    type: :boolean,
    doc: "If false, the value is set to `nil` if it's empty.",
    default: false
  ]
]
```

## Attributes with Constraints

To show how constraints can be used in a attribute, here is an example attribute describing a username:

```elixir
defmodule MyProject.MyDomain.Account do
  # ...

  code_interface do
    define :create, action: :create
  end

  actions do
    default [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key :id

    attribute :username, :string do
      constraints [
        max_length: 20,
        min_length: 3,
        match: ~r/^[a-z_-]*$/,
        trim?: true,
        allow_empty?: false
      ]
    end
  end

  # ...
end
```

If when creating or updating this attribute one of the constraints are not met, an error will be given telling you which constraint was broken. See below:

```elixir
iex(1)> MyProject.MyDomain.Account.create!(%{username: "hi"})

** (Ash.Error.Invalid) Input Invalid

* Invalid value provided for username: length must be greater than or equal to 3.

"hi"

iex(2)> MyProject.MyDomain.Account.create!(%{username: "Hello there this is a long string"})

** (Ash.Error.Invalid) Input Invalid

* Invalid value provided for username: length must be less than or equal to 20.

"Hello there this is a long string"

iex(3)> MyProject.MyDomain.Account.create!(%{username: "hello there"})
** (Ash.Error.Invalid) Input Invalid

* Invalid value provided for username: must match the pattern ~r/^[a-z_-]*$/.

"hello there"

iex(4)> MyProject.MyDomain.Account.create!(%{username: ""})
** (Ash.Error.Invalid) Input Invalid

* attribute title is required
```

It will give you the resource as usual on successful requests:

```elixir
iex(5)> MyProject.MyDomain.Account.create!(%{username: "hello"})
#MyProject.MyDomain.Account<
  __meta__: #Ecto.Schema.Metadata<:loaded, "account">,
  id: "7ba467dd-277c-4916-88ae-f62c93fee7a3",
  username: "hello",
  ...
>
```

## Arguments with Constraints

Arguments are used to input data into actions. As the data we pass in has a type we can apply constraints to validate the input arguments.

```elixir
defmodule MyProject.MyDomain.Account do
  # ...

  code_interface do
    define :create_username_with_age, action: :create_username_with_age
  end

  actions do
    default [:create, :read, :update, :destroy]

    create :create_username_with_age do
      argument :title, :string, allow_nil?: false

      argument :age, :integer do
        allow_nil? false
        constraints min: 18, max: 99
      end

      change fn changeset, _ ->
        title = Ash.Changeset.get_argument(changeset, :title)
        age = Ash.Changeset.get_argument(changeset, :age)

        Ash.Changeset.change_attribute(changeset, :username, "#{title}-#{age}")
      end
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :username, :string do
      constraints [
        max_length: 20,
        min_length: 3,
        match: ~r/^[a-z0-9_-]*$/,
        trim?: true,
        allow_empty?: false
      ]
    end
  end

  # ...
end
```

If you input argument is going to be used as a attribute directly, its best to put the constraint in the `attributes` block. But if you are combining multiple arguments to synthesize an attribute, then you should apply constraints to the arguments.

Above we have defined a custom action which takes 2 arguments `:title` and `:age` this action creates a username where the age of the user is embedded. However we have placed a limitation via the constraints so that only when age >= 18 and age <= 99 is the action allowed to occur. Lets see this in action.

```elixir
iex(1)> MyProject.MyDomain.Account.create_username_with_age!(%{username: "hello", age: 100})

** (Ash.Error.Invalid) Input Invalid

* Invalid value provided for age: must be less than or equal to 99.

100

iex(2)> MyProject.MyDomain.Account.create_username_with_age!(%{username: "hello", age: 99})
#MyProject.MyDomain.Account<
  __meta__: #Ecto.Schema.Metadata<:loaded, "accounts">,
  id: "5a28d5a1-25e6-4363-b173-3dd64e629dc8",
  title: "hello-99",
  ...
>
```
