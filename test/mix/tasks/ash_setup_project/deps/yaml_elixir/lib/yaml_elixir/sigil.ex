defmodule YamlElixir.Sigil do
  def sigil_y(string, [?a]), do: YamlElixir.read_from_string!(string, atoms: true)
  def sigil_y(string, []), do: YamlElixir.read_from_string!(string)
end
