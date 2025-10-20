# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.Policy do
  use ExUnit.Case, async: true

  import Crux.Expression, only: [b: 1]

  defmodule RuntimeCheck do
    @moduledoc false
    use Ash.Policy.Check

    @impl Ash.Policy.Check
    def describe(_), do: "returns true at runtime"

    @impl Ash.Policy.Check
    def strict_check(_, _, _), do: {:ok, :unknown}

    @impl Ash.Policy.Check
    def check(_actor, list, _map, _options), do: list
  end

  defmodule ErrorCheck do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    @impl Ash.Policy.Check
    def describe(_), do: "always errors"

    @impl Ash.Policy.SimpleCheck
    def match?(_actor, _authorizer, _options), do: {:error, :something_went_wrong}
  end

  describe inspect(&Ash.Policy.Policy.expression/1) do
    test "yields valid expression" do
      assert {Ash.Test.Policy.Policy.RuntimeCheck, []} =
               Ash.Policy.Policy.expression(
                 %Ash.Policy.Policy{
                   condition: [
                     {RuntimeCheck, []}
                   ],
                   policies: [
                     %Ash.Policy.Check{
                       check: {Ash.Policy.Check.Static, [result: true]},
                       check_module: Ash.Policy.Check.Static,
                       check_opts: [result: true, access_type: :filter],
                       type: :authorize_if
                     }
                   ]
                 },
                 %{resource: Resource}
               )
    end
  end

  describe inspect(&Ash.Policy.Policy.solve/1) do
    test "returns directly for runtime expandable policies" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        policies: [
          %Ash.Policy.Policy{
            condition: [
              {Ash.Policy.Check.ActorPresent, []}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true,
              %Ash.Policy.Authorizer{facts: %{{Ash.Policy.Check.ActorPresent, []} => true}}} =
               Ash.Policy.Policy.solve(%{authorization | actor: :actor})

      assert {:ok, false,
              %Ash.Policy.Authorizer{facts: %{{Ash.Policy.Check.ActorPresent, []} => false}}} =
               Ash.Policy.Policy.solve(%{authorization | actor: nil})
    end

    test "multiple non-bypass policies - all conditions match" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "multiple non-bypass policies - some conditions match" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: false]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "multiple non-bypass policies - no conditions match" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: nil,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "bypass policy alone - when it passes" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "bypass policy after passing policy" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "bypass policy alone - when condition fails" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: nil,
        policies: [
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "mix of bypass and regular policies - bypass succeeds and short-circuits" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "mix of bypass and regular policies - bypass fails, continues to regular" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: nil,
        policies: [
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "mutually exclusive conditions in same policy" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [
              {Ash.Policy.Check.ActorPresent, []},
              {Ash.Policy.Check.Static, [result: false]}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "forbid_if with true check forbids" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :forbid_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "forbid_if with false check does not forbid" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :forbid_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "authorize_unless with true check does not authorize" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_unless
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "authorize_unless with false check authorizes" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_unless
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "forbid_unless with true check does not forbid" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :forbid_unless
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "forbid_unless with false check forbids" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :forbid_unless
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "bypass with only forbid_if has no effect" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :forbid_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "multiple bypass policies - first succeeds" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "bypass policy after failing normal policy still fails" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.ActorPresent, []}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "mixed check types in single policy" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              },
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :forbid_if
              }
            ]
          }
        ]
      }

      assert {:ok, true, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "bypass with only forbid_unless has no effect" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            bypass?: true,
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :forbid_unless
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "empty policies list" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: []
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "authorize_if with false check" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        policies: [
          %Ash.Policy.Policy{
            condition: [{Ash.Policy.Check.Static, [result: true]}],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: false]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: false],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, _authorizer} = Ash.Policy.Policy.solve(authorization)
    end

    test "only checks conditions that are required" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        policies: [
          %Ash.Policy.Policy{
            condition: [
              {Ash.Policy.Check.ActorPresent, []},
              {Ash.Policy.Check.ActorAttributeEquals, [attribute: :role, value: :admin]}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, %Ash.Policy.Authorizer{facts: facts}} =
               Ash.Policy.Policy.solve(%{authorization | actor: nil})

      assert %{{Ash.Policy.Check.ActorPresent, []} => false} = facts

      refute Map.has_key?(
               facts,
               {Ash.Policy.Check.ActorAttributeEquals, [attribute: :role, value: :admin]}
             )

      assert {:ok, false, %Ash.Policy.Authorizer{facts: facts}} =
               Ash.Policy.Policy.solve(%{authorization | actor: %{role: :owner}})

      assert %{{Ash.Policy.Check.ActorPresent, []} => true} = facts

      assert %{
               {Ash.Policy.Check.ActorAttributeEquals, [attribute: :role, value: :admin]} => false
             } = facts

      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        policies: [
          %Ash.Policy.Policy{
            condition: [
              {Ash.Policy.Check.ActorPresent, []}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.ActorAttributeEquals, [attribute: :role, value: :admin]},
                check_module: Ash.Policy.Check.ActorAttributeEquals,
                check_opts: [attribute: :role, value: :admin],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, false, %Ash.Policy.Authorizer{facts: facts}} =
               Ash.Policy.Policy.solve(%{authorization | actor: nil})

      assert %{{Ash.Policy.Check.ActorPresent, []} => false} = facts

      refute Map.has_key?(
               facts,
               {Ash.Policy.Check.ActorAttributeEquals, [attribute: :role, value: :admin]}
             )

      assert {:ok, false, %Ash.Policy.Authorizer{facts: facts}} =
               Ash.Policy.Policy.solve(%{authorization | actor: %{role: :owner}})

      assert %{{Ash.Policy.Check.ActorPresent, []} => true} = facts

      assert %{
               {Ash.Policy.Check.ActorAttributeEquals, [attribute: :role, value: :admin]} => false
             } = facts
    end

    test "declares scenarios and their requirements for uncertain output" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        policies: [
          %Ash.Policy.Policy{
            condition: [
              {RuntimeCheck, []}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [
              {RuntimeCheck, []}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          },
          %Ash.Policy.Policy{
            condition: [
              {RuntimeCheck, [some: :config]}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:ok, required_conditions, _authorizer} =
               Ash.Policy.Policy.solve(%{authorization | actor: nil})

      assert Enum.any?(required_conditions, fn facts ->
               Map.get(facts, {Ash.Test.Policy.Policy.RuntimeCheck, []}) == true
             end)

      assert Enum.any?(required_conditions, fn facts ->
               Map.get(facts, {Ash.Test.Policy.Policy.RuntimeCheck, [some: :config]}) == true
             end)
    end

    test "returns error if check failed" do
      authorization = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        policies: [
          %Ash.Policy.Policy{
            condition: [
              {ErrorCheck, []}
            ],
            policies: [
              %Ash.Policy.Check{
                check: {Ash.Policy.Check.Static, [result: true]},
                check_module: Ash.Policy.Check.Static,
                check_opts: [result: true],
                type: :authorize_if
              }
            ]
          }
        ]
      }

      assert {:error, _authorizer,
              %Ash.Error.Unknown.UnknownError{
                class: :unknown,
                error: "unknown error: :something_went_wrong"
              }} =
               Ash.Policy.Policy.solve(%{authorization | actor: nil})
    end
  end

  describe inspect(&Ash.Policy.Policy.transform/1) do
    test "keeps good policy untouched" do
      policy = %Ash.Policy.Policy{
        condition: [
          {Ash.Policy.Check.Static, [result: false]}
        ],
        policies: [
          %Ash.Policy.Check{
            check: {Ash.Policy.Check.Static, [result: true]},
            check_module: Ash.Policy.Check.Static,
            check_opts: [result: true],
            type: :authorize_if
          }
        ]
      }

      assert {:ok, ^policy} = Ash.Policy.Policy.transform(policy)

      bypass_policy = %Ash.Policy.Policy{
        bypass?: true,
        condition: [
          {Ash.Policy.Check.Static, [result: false]}
        ],
        policies: [
          %Ash.Policy.Check{
            check: {Ash.Policy.Check.Static, [result: true]},
            check_module: Ash.Policy.Check.Static,
            check_opts: [result: true],
            type: :authorize_if
          }
        ]
      }

      assert {:ok, ^bypass_policy} = Ash.Policy.Policy.transform(bypass_policy)
    end

    test "errors with no checks" do
      assert {:error, "Policies must have at least one check."} =
               Ash.Policy.Policy.transform(%Ash.Policy.Policy{})
    end

    test "does not allow bypass with only forbid" do
      assert {:error, msg} =
               Ash.Policy.Policy.transform(%Ash.Policy.Policy{
                 bypass?: true,
                 condition: [
                   {Ash.Policy.Check.Static, [result: false]}
                 ],
                 policies: [
                   %Ash.Policy.Check{
                     check: {Ash.Policy.Check.Static, [result: false]},
                     check_module: Ash.Policy.Check.Static,
                     check_opts: [result: false],
                     type: :forbid_if
                   }
                 ]
               })

      assert msg =~
               "This policy only contains `forbid_if` or `forbid_unless` check types therefore"
    end

    test "adds empty condition if none given" do
      assert {:ok, %{condition: [{Ash.Policy.Check.Static, result: true}]}} =
               Ash.Policy.Policy.transform(%Ash.Policy.Policy{
                 policies: [
                   %Ash.Policy.Check{
                     check: {Ash.Policy.Check.Static, [result: true]},
                     check_module: Ash.Policy.Check.Static,
                     check_opts: [result: true],
                     type: :authorize_if
                   }
                 ]
               })
    end
  end

  describe inspect(&Ash.Policy.Policy.fetch_or_strict_check_fact/2) do
    test "calculates and stores new check content" do
      check = {Ash.Policy.Check.ActorPresent, []}

      authorizer = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        facts: %{}
      }

      assert Ash.Policy.Policy.fetch_or_strict_check_fact(authorizer, check) ==
               {:ok, true, %{authorizer | facts: %{check => true}}}
    end

    test "calculates and does not store static content" do
      check = {Ash.Policy.Check.Static, [result: true]}

      authorizer = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        actor: :actor,
        facts: %{}
      }

      assert Ash.Policy.Policy.fetch_or_strict_check_fact(authorizer, check) ==
               {:ok, true, authorizer}
    end

    test "gives stored fact" do
      check = {RuntimeCheck, []}

      authorizer = %Ash.Policy.Authorizer{
        resource: Resource,
        action: :read,
        facts: %{check => false}
      }

      assert Ash.Policy.Policy.fetch_or_strict_check_fact(authorizer, check) ==
               {:ok, false, authorizer}
    end
  end

  describe inspect(&Ash.Policy.Policy.fetch_fact/2) do
    test "gives stored fact" do
      assert {:ok, false} =
               Ash.Policy.Policy.fetch_fact(%{{RuntimeCheck, []} => false}, {RuntimeCheck, []})
    end

    test "ignores access_type field" do
      assert {:ok, false} =
               Ash.Policy.Policy.fetch_fact(
                 %{{RuntimeCheck, []} => false},
                 {RuntimeCheck, [access_type: :runtime]}
               )
    end

    test "gives error if missing" do
      assert :error = Ash.Policy.Policy.fetch_fact(%{}, {RuntimeCheck, []})
    end
  end

  describe inspect(&Ash.Policy.Policy.debug_expr/2) do
    test "generates readable expression" do
      assert Ash.Policy.Policy.debug_expr(
               b({RuntimeCheck, []} and ({RuntimeCheck, [some: :config]} or not true))
             ) == """
             Expr:

             "returns true at runtime" and ("returns true at runtime" or not true)\
             """
    end
  end

  describe "bypass policy with always-true condition" do
    test "denies when bypass condition is true but bypass policies fail" do
      policies = [
        %Ash.Policy.Policy{
          bypass?: true,
          condition: [{Ash.Policy.Check.Static, result: true}],
          policies: [
            %Ash.Policy.Check{
              type: :authorize_if,
              check: {Ash.Policy.Check.Static, result: false},
              check_module: Ash.Policy.Check.Static,
              check_opts: [result: false]
            }
          ]
        },
        %Ash.Policy.Policy{
          bypass?: false,
          condition: [{Ash.Policy.Check.Static, result: false}],
          policies: [
            %Ash.Policy.Check{
              type: :authorize_if,
              check: {Ash.Policy.Check.Static, result: true},
              check_module: Ash.Policy.Check.Static,
              check_opts: [result: true]
            }
          ]
        }
      ]

      expression = Ash.Policy.Policy.expression(policies, %{})

      assert expression == false
    end
  end
end
