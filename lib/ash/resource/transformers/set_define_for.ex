# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.SetDefineFor do
  @moduledoc false
  use Spark.Dsl.Transformer

  def transform(dsl) do
    domain = Spark.Dsl.Transformer.get_persisted(dsl, :domain)

    if domain && !Ash.Resource.Info.code_interface_domain(dsl) do
      {:ok, Spark.Dsl.Transformer.set_option(dsl, [:code_interface], :domain, domain)}
    else
      {:ok, dsl}
    end
  end
end
