# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Template do
  @moduledoc """
  Templates used to refer to some sort of computed value.
  """
  alias __MODULE__.{Element, Input, Result, Value}

  @type t :: Element.t() | Input.t() | Result.t() | Value.t()

  @doc "The type for use in option schemas"
  @spec type :: Spark.Options.type()
  def type, do: {:or, [{:struct, Element}, {:struct, Input}, {:struct, Result}, {:struct, Value}]}

  @doc "A guard for input templates"
  @spec is_input_template(any) :: Macro.output()
  defguard is_input_template(template) when is_struct(template, Input)

  @doc "A guard for result templates"
  @spec is_result_template(any) :: Macro.output()
  defguard is_result_template(template) when is_struct(template, Result)

  @doc "A guard for value templates"
  @spec is_value_template(any) :: Macro.output()
  defguard is_value_template(template) when is_struct(template, Value)

  @doc "A guard for element templates"
  @spec is_element_template(any) :: Macro.output()
  defguard is_element_template(template) when is_struct(template, Element)

  @doc "A guard to detect all template types"
  @spec is_template(any) :: Macro.output()
  defguard is_template(template)
           when is_input_template(template) or is_result_template(template) or
                  is_value_template(template) or is_element_template(template)
end
