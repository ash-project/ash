defmodule Ash.Test.Type.UnionTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Foo do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :foo, :string, constraints: [match: ~r/foo/], public?: true

      attribute :type, :string do
        public?(true)
        writable? false
        default "foo"
      end
    end
  end

  defmodule Bar do
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute :bar, :string, constraints: [match: ~r/bar/], public?: true

      attribute :type, :string do
        public?(true)
        writable? false
        default "bar"
      end
    end
  end

  defmodule FooBarUnion do
    use Ash.Type.NewType,
      subtype_of: :union,
      constraints: [
        types: [
          foo: [
            type: Foo,
            cast_tag?: false,
            tag: :type,
            tag_value: :foo
          ],
          bar: [
            type: Bar,
            cast_tag?: false,
            tag: :type,
            tag_value: :bar
          ]
        ]
      ]
  end

  defmodule Example do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:create, :read, :update, :destroy]

      update :add_thing do
        argument :new_thing, FooBarUnion, allow_nil?: false

        change fn changeset, _ ->
          new_thing =
            Ash.Changeset.get_argument(changeset, :new_thing)

          things = Ash.Changeset.get_attribute(changeset, :things)

          Ash.Changeset.change_attribute(
            changeset,
            :things,
            things ++ [new_thing]
          )
        end
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :things, {:array, :union} do
        public?(true)

        constraints items: [
                      types: [
                        foo: [
                          type: Foo,
                          cast_tag?: false,
                          tag: :type,
                          tag_value: :foo
                        ],
                        bar: [
                          type: Bar,
                          cast_tag?: false,
                          tag: :type,
                          tag_value: :bar
                        ]
                      ]
                    ]
      end

      attribute :thing, :union,
        public?: true,
        constraints: [
          types: [
            foo: [
              type: Foo,
              cast_tag?: false,
              tag: :type,
              tag_value: "foo"
            ],
            bar: [
              type: Bar,
              cast_tag?: false,
              tag: :type,
              tag_value: "bar"
            ]
          ]
        ]
    end
  end

  test "it handles simple types" do
    constraints = [
      types: [
        int: [
          type: :integer,
          constraints: [
            max: 10
          ]
        ],
        string: [
          type: :string
        ]
      ]
    ]

    {:ok, %{constraints: constraints}} =
      Ash.Type.set_type_transformation(%{type: Ash.Type.Union, constraints: constraints})

    assert {:ok, %Ash.Union{value: 1, type: :int}} = Ash.Type.cast_input(:union, 1, constraints)

    assert {:error, _} = Ash.Type.cast_input(:union, 11, constraints)
  end

  test "it handles changes between native types" do
    constraints = [
      types: [
        foo: [
          type: :integer
        ],
        bar: [
          type: :string
        ]
      ]
    ]

    assert {:ok, %Ash.Union{type: :bar, value: "bar"}} =
             Ash.Type.Union.prepare_change(
               %Ash.Union{type: :foo, value: 1},
               "bar",
               constraints
             )

    assert {:ok, %Ash.Union{type: :bar, value: "bar"}} =
             Ash.Type.Union.handle_change(
               %Ash.Union{type: :foo, value: 1},
               %Ash.Union{type: :bar, value: "bar"},
               constraints
             )

    assert {:ok, %Ash.Union{type: :bar, value: "bar"}} =
             Ash.Type.Union.handle_change(
               nil,
               %Ash.Union{type: :bar, value: "bar"},
               constraints
             )

    assert {:ok, %Ash.Union{type: :bar, value: "bar2"}} =
             Ash.Type.Union.handle_change(
               %Ash.Union{type: :bar, value: "bar1"},
               %Ash.Union{type: :bar, value: "bar2"},
               constraints
             )

    assert {:ok, %Ash.Union{value: nil, type: :bar}} =
             Ash.Type.Union.handle_change(
               %Ash.Union{type: :bar, value: "bar1"},
               nil,
               constraints
             )
  end

  test "it handles changes between native and tagged types" do
    constraints = [
      types: [
        foo: [
          type: Foo,
          cast_tag?: false,
          constraints: [domain: Domain],
          tag: :type,
          tag_value: :foo
        ],
        bar: [
          type: :string
        ]
      ]
    ]

    assert {:ok, %Ash.Union{type: :bar, value: "bar"}} =
             Ash.Type.Union.prepare_change(
               %Ash.Union{type: :foo, value: %Foo{}},
               "bar",
               constraints
             )

    assert {:ok, %Ash.Union{type: :bar, value: "bar"}} =
             Ash.Type.Union.handle_change(
               %Ash.Union{type: :foo, value: %{}},
               %Ash.Union{type: :bar, value: "bar"},
               constraints
             )
  end

  test "it handles tagged types" do
    constraints = [
      types: [
        foo: [
          type: :map,
          tag: :type,
          tag_value: :foo
        ],
        bar: [
          type: :map,
          tag: :type,
          tag_value: :bar
        ]
      ]
    ]

    {:ok, %{constraints: constraints}} =
      Ash.Type.set_type_transformation(%{type: Ash.Type.Union, constraints: constraints})

    assert {:ok, %Ash.Union{value: %{type: :foo, bar: 1}, type: :foo}} =
             Ash.Type.cast_input(:union, %{type: :foo, bar: 1}, constraints)

    assert {:ok, %Ash.Union{value: %{type: :bar, bar: 1}, type: :bar}} =
             Ash.Type.cast_input(:union, %{type: :bar, bar: 1}, constraints)

    assert {:error, _} =
             Ash.Type.cast_input(:union, %{type: :baz, bar: 1}, constraints)
  end

  test "it handles paths" do
    constraints = [
      items: [
        types: [
          foo: [
            type: Foo,
            cast_tag?: false,
            constraints: [domain: Domain],
            tag: :type,
            tag_value: :foo
          ],
          bar: [
            type: Bar,
            cast_tag?: false,
            constraints: [domain: Domain],
            tag: :type,
            tag_value: :bar
          ]
        ]
      ]
    ]

    {:ok, %{type: type, constraints: constraints}} =
      Ash.Type.set_type_transformation(%{
        type: {:array, Ash.Type.Union},
        constraints: constraints
      })

    assert {:ok, [%Ash.Union{value: %{type: "foo", foo: "foo"}, type: :foo}]} =
             Ash.Type.cast_input(type, [%{type: :foo, foo: "foo"}], constraints)

    assert {:error,
            [
              error
            ]} =
             Ash.Type.cast_input(type, [%{type: :foo, foo: "bar"}], constraints)

    for {key, val} <- [
          message: "must match the pattern %{regex}",
          field: :foo,
          regex: "~r/foo/",
          index: 0,
          path: [0]
        ] do
      assert error[key] == val
    end
  end

  test "it handles changing union attribute on a resource" do
    Example
    |> Ash.Changeset.for_create(:create, %{thing: %Foo{type: "foo", foo: "foo"}})
    |> Ash.create!()
    |> Ash.Changeset.new()
    |> Ash.Changeset.change_attribute(:thing, %Bar{type: "bar", bar: "bar"})
    |> Ash.Changeset.for_update(:update)
    |> Ash.update!()
  end

  test "it handles paths on a resource" do
    Example
    |> Ash.Changeset.for_create(:create, %{things: [%{type: :foo, foo: "bar"}]})
    |> Ash.create()
  end

  test "it dumps to native as explicit maps by default" do
    constraints = [
      types: [
        foo: [
          type: :map,
          tag: :type,
          tag_value: :foo
        ],
        bar: [
          type: :map,
          tag: :type,
          tag_value: :bar
        ]
      ]
    ]

    {:ok, %{constraints: constraints}} =
      Ash.Type.set_type_transformation(%{type: Ash.Type.Union, constraints: constraints})

    assert {:ok, %Ash.Union{value: %{type: :foo, bar: 1}, type: :foo} = union} =
             Ash.Type.cast_input(:union, %{type: :foo, bar: 1}, constraints)

    assert Ash.Type.dump_to_native(Ash.Type.Union, union, constraints) ==
             {:ok, %{"type" => :foo, "value" => %{type: :foo, bar: 1}}}
  end

  test "it dumps to native as raw maps when configured" do
    constraints = [
      storage: :map_with_tag,
      types: [
        foo: [
          type: :map,
          tag: :type,
          tag_value: :foo
        ],
        bar: [
          type: :map,
          tag: :type,
          tag_value: :bar
        ]
      ]
    ]

    {:ok, %{constraints: constraints}} =
      Ash.Type.set_type_transformation(%{type: Ash.Type.Union, constraints: constraints})

    assert {:ok, %Ash.Union{value: %{type: :foo, bar: 1}, type: :foo} = union} =
             Ash.Type.cast_input(:union, %{type: :foo, bar: 1}, constraints)

    assert Ash.Type.dump_to_native(Ash.Type.Union, union, constraints) ==
             {:ok, %{type: :foo, bar: 1}}
  end

  test "it should cast union arguments appropriately" do
    assert {:ok, _} =
             Example
             |> Ash.Changeset.for_create(:create, %{things: []})
             |> Ash.create!()
             |> Ash.Changeset.for_update(:add_thing, %{new_thing: %{type: :foo, foo: "foo"}})
             |> Ash.update()
  end
end
