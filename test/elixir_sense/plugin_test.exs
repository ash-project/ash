defmodule Ash.ElixirSense.PluginTest do
  use ExUnit.Case

  def cursors(text) do
    {_, cursors} =
      ElixirSense.Core.Source.walk_text(text, {false, []}, fn
        "#", rest, _, _, {_comment?, cursors} ->
          {rest, {true, cursors}}

        "\n", rest, _, _, {_comment?, cursors} ->
          {rest, {false, cursors}}

        "^", rest, line, col, {true, cursors} ->
          {rest, {true, [%{line: line - 1, col: col} | cursors]}}

        _, rest, _, _, acc ->
          {rest, acc}
      end)

    Enum.reverse(cursors)
  end

  def suggestions(buffer, cursor) do
    ElixirSense.suggestions(buffer, cursor.line, cursor.col)
  end

  def suggestions(buffer, cursor, type) do
    suggestions(buffer, cursor)
    |> Enum.filter(fn s -> s.type == type end)
  end

  def suggestions_by_kind(buffer, cursor, kind) do
    suggestions(buffer, cursor)
    |> Enum.filter(fn s -> s[:kind] == kind end)
  end

  test "suggesting DSL items" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource

      attrib
      #     ^
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               detail: "Dsl Section",
               documentation: doc,
               kind: :function,
               label: "attributes",
               snippet: "attributes do\n  $0\nend",
               type: :generic
             }
           ] = result

    assert doc =~ "attributes"
  end

  test "autocomplete remains when cursor is at the end of the word" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource

      attributes
      #         ^
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               detail: "Dsl Section",
               documentation: doc,
               kind: :function,
               label: "attributes",
               snippet: "attributes do\n  $0\nend",
               type: :generic
             }
           ] = result

    assert doc =~ "attributes"
  end

  test "autocomplete can detect recursive sections" do
    buffer = """
    defmodule MyFlow do
      use Ash.Flow

      steps do
        map :name do
          cre
    #        ^
        end
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               detail: "Dsl Entity",
               documentation: doc,
               kind: :function,
               label: "create",
               type: :generic
             }
           ] = result

    assert doc =~ "create"
  end

  test "autocomplete can detect recursive sections deeply" do
    buffer = """
    defmodule MyFlow do
      use Ash.Flow

      steps do
        map :name, :over do
          transaction :transaction, Foobar do
            map :name, :over do
               tra
    #             ^
            end
          end
        end
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               detail: "Dsl Entity",
               documentation: doc,
               kind: :function,
               label: "transaction",
               type: :generic
             }
           ] = result

    assert doc =~ "transaction"
  end

  test "suggesting DSL options in a section" do
    buffer = """
    defmodule MyResource do
      @moduledoc false
      use Ash.Resource

      resource do
        descri
        #     ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               detail: "Option",
               documentation:
                 "A human readable description of the resource, to be used in generated documentation",
               kind: :function,
               label: "description",
               snippet: "description \"$0\"",
               type: :generic
             }
           ] = result
  end

  test "suggesting available sections" do
    buffer = """
    defmodule do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      # ^
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    labels =
      result |> Enum.filter(&Map.has_key?(&1, :label)) |> Enum.map(& &1.label) |> Enum.sort()

    assert labels == [
             "actions",
             "aggregates",
             "attributes",
             "calculations",
             "changes",
             "code_interface",
             "identities",
             "multitenancy",
             "preparations",
             "pub_sub",
             "relationships",
             "resource",
             "validations"
           ]
  end

  test "suggesting available nested sections" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      actions do

      # ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)
    labels = result |> Enum.map(& &1.label) |> Enum.sort()

    assert labels == ["create", "defaults", "destroy", "read", "update"]
  end

  test "suggesting available options and entities" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      actions do
        create do

        # ^
        end
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)
    labels = result |> Enum.map(& &1.label) |> Enum.sort()

    assert labels == [
             "accept",
             "allow_nil_input",
             "argument",
             "change",
             "description",
             "error_handler",
             "manual?",
             "metadata",
             "name",
             "primary?",
             "reject",
             "require_attributes",
             "touches_resources",
             "transaction?",
             "validate"
           ]
  end

  test "suggesting available options when in keyword format" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      attributes do
        attribute :name, :type, all
    #                              ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               detail: "Option",
               kind: :function,
               label: "always_select?",
               snippet: "always_select?: true",
               type: :generic
             },
             %{
               detail: "Option",
               kind: :function,
               label: "allow_nil?",
               snippet: "allow_nil?: false",
               type: :generic
             }
           ] = result
  end

  test "suggesting available values when in keyword format" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      attributes do
        attribute :name, :type, allow_nil?:
    #                                       ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               documentation: "true",
               insert_text: "true",
               kind: :type_parameter,
               label: "boolean",
               priority: 0,
               snippet: "true",
               type: :generic
             },
             %{
               documentation: "false",
               insert_text: "false",
               kind: :type_parameter,
               label: "boolean",
               priority: 0,
               snippet: "false",
               type: :generic
             }
           ] = result
  end

  test "suggesting available values in section" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      attributes do
        attribute :name, :type do
          allow_nil?
    #                ^
        end
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               documentation: "true",
               insert_text: "true",
               kind: :type_parameter,
               label: "boolean",
               priority: 0,
               snippet: "true",
               type: :generic
             },
             %{
               documentation: "false",
               insert_text: "false",
               kind: :type_parameter,
               label: "boolean",
               priority: 0,
               snippet: "false",
               type: :generic
             }
           ] = result
  end

  test "suggesting values in args with no hint" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      pub_sub do
        publish_all
      #             ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               documentation: ":create",
               insert_text: ":create",
               kind: :type_parameter,
               label: "type",
               priority: 0,
               snippet: ":create",
               type: :generic
             },
             %{
               documentation: ":update",
               insert_text: ":update",
               kind: :type_parameter,
               label: "type",
               priority: 0,
               snippet: ":update",
               type: :generic
             },
             %{
               documentation: ":destroy",
               insert_text: ":destroy",
               kind: :type_parameter,
               label: "type",
               priority: 0,
               snippet: ":destroy",
               type: :generic
             }
           ] = result
  end

  test "suggesting values in args with hint" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      pub_sub do
        publish_all :crea
      #                  ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               documentation: ":create",
               insert_text: ":create",
               kind: :type_parameter,
               label: "type",
               priority: 0,
               snippet: ":create",
               type: :generic
             }
           ] = result
  end

  test "suggesting ash builtin types" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      attributes do
        attribute :name, :str
        #                    ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               detail: "Ash type",
               insert_text: "string",
               kind: :type_parameter,
               label: ":string",
               priority: 0,
               snippet: nil,
               type: :generic
             },
             %{
               detail: "Ash type",
               insert_text: "ci_string",
               kind: :type_parameter,
               label: ":ci_string",
               priority: 0,
               snippet: nil,
               type: :generic
             }
           ] = result
  end

  test "suggesting builtin types for behaviours" do
    buffer = """
    defmodule MyResource do
      use Ash.Resource,
        extensions: [Ash.Notifier.PubSub]

      actions do
        create :create do
          change manage_relati
          #                   ^
        end
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert [
             %{
               args: "argument, relationship_name \\\\ nil, opts",
               name: "manage_relationship",
               args_list: ["argument", "relationship_name \\\\ nil", "opts"],
               arity: 2,
               def_arity: 3,
               metadata: %{defaults: 1},
               origin: "Ash.Resource.Change.Builtins",
               snippet: nil,
               spec: "",
               summary:
                 "Calls `Ash.Changeset.manage_relationship/4` with the changeset and relationship provided, using the value provided for the named argument" <>
                   _,
               type: :function,
               visibility: :public
             },
             %{
               args: "argument, relationship_name \\\\ nil, opts",
               args_list: ["argument", "relationship_name \\\\ nil", "opts"],
               arity: 3,
               def_arity: 3,
               metadata: %{defaults: 1},
               name: "manage_relationship",
               origin: "Ash.Resource.Change.Builtins",
               snippet: nil,
               spec: "",
               summary:
                 "Calls `Ash.Changeset.manage_relationship/4` with the changeset and relationship provided, using the value provided for the named argument" <>
                   _,
               type: :function,
               visibility: :public
             }
           ] = result
  end

  test "suggesting available sections from single extension types" do
    buffer = """
    defmodule do
      use Ash.Resource,
        data_layer: Ash.DataLayer.Ets

      ets do

    # ^
      end
    end
    """

    [cursor] = cursors(buffer)

    result = suggestions(buffer, cursor)

    assert result == [
             %{
               detail: "Option",
               documentation: "Sets the ets table protection to private",
               kind: :function,
               label: "private?",
               snippet: "private? true",
               type: :generic
             },
             %{
               detail: "Option",
               documentation: "The name of the table. Defaults to the resource name.\n",
               kind: :function,
               label: "table",
               snippet: "table :$0",
               type: :generic
             }
           ]
  end
end
