# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Relationships.ManualTestMacros do
  defmacro defresources(
             simple?,
             multi?,
             many?,
             do: body
           ) do
    post_attributes =
      quote do
        case {unquote(simple?), unquote(multi?)} do
          {true, true} ->
            [%{name: :number, type: :string}, %{name: :number2, type: :string}]

          {true, false} ->
            [%{name: :number, type: :string}]

          {false, true} ->
            [
              %{name: :number, type: :ci_string},
              %{name: :number2, type: :ci_string}
            ]

          {false, false} ->
            [%{name: :number, type: :ci_string}]
        end
      end

    quote do
      module_pfx = "rand#{System.unique_integer([:positive])}"
      comment_module = Module.concat([module_pfx, Comment])
      use_context_rel_module = Module.concat([module_pfx, UseContextRelationship])
      post_module = Module.concat([module_pfx, Post])

      defmodule comment_module do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        ets do
          private?(true)
        end

        actions do
          default_accept :*
          defaults [:read, :destroy, create: :*, update: :*]
        end

        attributes do
          uuid_primary_key :id
          attribute :number, :string, allow_nil?: false, public?: true
        end
      end

      @comment_module comment_module

      defmodule use_context_rel_module do
        @moduledoc false
        use Ash.Resource.ManualRelationship

        @impl true
        def load(_posts, _opts, %{source_context: %{private: %{load_result: load_result}}}) do
          {:ok, load_result}
        end
      end

      defmodule post_module do
        @moduledoc false
        use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

        actions do
          default_accept :*
          defaults [:read, :destroy, create: :*, update: :*]
        end

        attributes do
          for config <- unquote(post_attributes) do
            attribute config.name, config.type do
              allow_nil? false
              public? true
              primary_key? true
            end
          end
        end

        relationships do
          if unquote(many?) do
            has_many :use_ctx_comment, comment_module do
              public? true
              manual use_context_rel_module
              no_attributes? true
            end
          else
            has_one :use_ctx_comment, comment_module do
              public? true
              allow_nil? true
              manual use_context_rel_module
              no_attributes? true
            end
          end
        end
      end

      @post_module post_module

      unquote(body)
    end
  end

  defmacro run_test(
             post_module,
             comment_module,
             post_inputs,
             load_result,
             expected,
             multi?,
             many?
           ) do
    post_inputs =
      Macro.escape(
        Enum.map(post_inputs, fn number ->
          if multi? do
            %{number: number, number2: number}
          else
            %{number: number}
          end
        end)
      )

    load_result =
      Macro.escape(
        if is_map(load_result) do
          Map.new(load_result, fn {post_number, comment_number} ->
            post_number =
              if multi? do
                %{number: post_number, number2: post_number}
              else
                post_number
              end

            {post_number, comment_number}
          end)
        else
          load_result
        end
      )

    create_comment_fn =
      quote do
        fn num ->
          num =
            case num do
              nil -> nil
              num -> Ash.create!(unquote(comment_module), %{number: num})
            end

          unquote(
            if many? do
              quote do
                List.wrap(num)
              end
            else
              quote(do: num)
            end
          )
        end
      end

    expected =
      Macro.escape(
        Enum.map(expected, fn
          nil ->
            %{use_ctx_comment: if(many?, do: [], else: nil)}

          num ->
            %{use_ctx_comment: if(many?, do: [%{number: num}], else: %{number: num})}
        end)
      )

    quote do
      posts =
        Enum.map(unquote(post_inputs), fn post_input ->
          if post_input == nil do
            nil
          else
            Ash.create!(unquote(post_module), post_input)
          end
        end)

      create_comment = unquote(create_comment_fn)

      load_result =
        case unquote(load_result) do
          res when is_map(res) ->
            Map.new(res, fn {post_number, comment_number} ->
              {post_number, create_comment.(comment_number)}
            end)

          res when is_list(res) ->
            Enum.map(res, create_comment)
        end

      posts =
        Ash.load!(posts, [:use_ctx_comment], context: %{private: %{load_result: load_result}})

      assert unquote(expected) = posts
    end
  end
end

defmodule Ash.Test.Resource.Relationships.ManualTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test.Resource.Relationships.ManualTestMacros

  @test_cases [
    %{
      tag: "1>1",
      name: "it associates a single record -> single record",
      posts: ["0"],
      load_results: %{map: %{"0" => "0"}, list: ["0"]},
      expected: ["0"]
    },
    %{
      tag: "1>nil",
      name: "it associates a single record -> nil",
      posts: ["0"],
      load_results: %{map: %{"0" => nil}, list: [nil]},
      expected: [nil]
    },
    %{
      tag: "1>none",
      name: "it associates a single record -> no match",
      posts: ["0"],
      load_results: %{map: %{}, list: []},
      expected: [nil]
    },
    %{
      tag: "3>3",
      name: "it associates three records -> three records",
      posts: ["0", "1", "2"],
      load_results: %{map: %{"0" => "0", "1" => "1", "2" => "2"}, list: ["0", "1", "2"]},
      expected: ["0", "1", "2"]
    },
    %{
      tag: "3>nils",
      name: "it associates three records -> three nils",
      posts: ["0", "1", "2"],
      load_results: %{map: %{"0" => nil, "1" => nil, "2" => nil}, list: [nil, nil, nil]},
      expected: [nil, nil, nil]
    },
    %{
      tag: "3>none",
      name: "it associates three records -> no matches",
      posts: ["0", "1", "2"],
      load_results: %{map: %{}, list: []},
      expected: [nil, nil, nil]
    },
    %{
      tag: "3>mixed",
      name: "it associates three records -> mixed records/nils",
      posts: ["0", "1", "2"],
      load_results: %{map: %{"0" => "0", "1" => nil, "2" => "2"}, list: ["0", nil, "2"]},
      expected: ["0", nil, "2"]
    },
    %{
      tag: "3>none+nil",
      name: "it associates three records -> mixed records/nils (with one missing)",
      posts: ["0", "1", "2"],
      load_results: %{map: %{"0" => "0", "2" => nil}},
      expected: ["0", nil, nil]
    },
    %{
      tag: "1>2",
      name: "it associates a single record -> single record (even with >1 returned)",
      posts: ["0"],
      load_results: %{list: ["0", "1"]},
      expected: ["0"]
    },
    %{
      tag: "3>2",
      name: "it associates three records -> two records, using nil for last",
      posts: ["0", "1", "2"],
      load_results: %{list: ["0", "1"]},
      expected: ["0", "1", nil]
    }
  ]

  @strategies_to_test [:map, :list]

  for simple? <- [true, false],
      multi? <- [true, false],
      many? <- [true, false] do
    defresources simple?, multi?, many? do
      for test_case <- @test_cases,
          valid_load_results =
            test_case.load_results
            |> Map.take(@strategies_to_test),
          {strategy, load_result} <- valid_load_results do
        tag_str =
          "#{test_case.tag} strategy=#{strategy} simple?=#{simple?} multi?=#{multi?} many?=#{many?}"

        @tag actions_has_one: tag_str
        @name "#{test_case.name} #{tag_str}"
        test @name do
          run_test(
            unquote(@post_module),
            unquote(@comment_module),
            unquote(test_case.posts),
            unquote(load_result),
            unquote(test_case.expected),
            unquote(multi?),
            unquote(many?)
          )
        end
      end
    end
  end
end
