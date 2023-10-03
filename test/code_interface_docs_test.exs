defmodule Ash.Test.CodeInterfaceDocsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defp fetch_function_doc_content(module, function_name, arity, lang) do
    {_, _, _, _, _, _, function_docs} = Code.fetch_docs(module)

    Enum.reduce_while(
      function_docs,
      nil,
      fn
        {{:function, ^function_name, ^arity}, _, _signature, content, _}, _acc
        when is_map_key(content, lang) ->
          {:halt, content[lang]}

        _, acc ->
          {:cont, acc}
      end
    )
  end

  # defp fetch_interface(module, function_name) do
  #   module
  #   |> Ash.Resource.Info.interfaces()
  #   |> Enum.find([], fn %Ash.Resource.Interface{name: name} -> name == function_name end)
  # end

  describe "Code interface doc content" do
    test "exists for all interface functions" do
      interface_functions =
        Ash.Test.User
        |> Ash.Resource.Info.interfaces()
        |> Enum.reduce([], fn %Ash.Resource.Interface{name: name}, names ->
          [name | [String.to_existing_atom(Atom.to_string(name) <> "!") | names]]
        end)

      {_, _, _, _, _, _, function_docs} = Code.fetch_docs(Ash.Test.User)

      functions_with_docs =
        function_docs
        |> Enum.reduce(
          MapSet.new(),
          fn
            {{:function, name, _}, _, _, content, _}, function_elems when map_size(content) > 0 ->
              MapSet.put(function_elems, name)

            _, function_elems ->
              function_elems
          end
        )

      for function <- interface_functions do
        assert MapSet.member?(functions_with_docs, function)
      end
    end

    test "includes the name of the action" do
      assert fetch_function_doc_content(Ash.Test.User, :welcome, 4, "en") =~ "welcome"
    end

    test "includes names and descriptions of the args of the action" do
      welcome_doc = fetch_function_doc_content(Ash.Test.User, :welcome, 4, "en")

      assert welcome_doc =~ "* greeting"
      assert welcome_doc =~ "* name - The name of the person to be greeted"
    end

    test "includes names and descriptions of the accepts of the action" do
      create_doc = fetch_function_doc_content(Ash.Test.User, :create, 3, "en")

      assert create_doc =~ "* first_name"
      assert create_doc =~ "* last_name - The user's last name"
    end
  end
end
