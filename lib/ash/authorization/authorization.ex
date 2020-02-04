defmodule Ash.Authorization do
  @moduledoc """
  #TODO: Explain authorization

  Authorization in Ash is done via declaring `rules` for actions,
  and in the case of stateful actions, via declaring `authoriation_steps` on attributes
  and relationships.


  # TODO: consider this coverage metric when building the test framework
  https://en.wikipedia.org/wiki/Modified_condition/decision_coverage
  """

  @type request :: Ash.Engine.Request.t()

  @type side_load :: {:side_load, Keyword.t()}
  @type prepare_instruction :: side_load
end
