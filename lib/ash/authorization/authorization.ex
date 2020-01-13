defmodule Ash.Authorization do
  @moduledoc """
  #TODO: Explain authorization

  Authorization in Ash is done via declaring `rules` for actions,
  and in the case of stateful actions, via declaring `authoriation_steps` on attributes
  and relationships.

  In the case of `read` actions
  """

  @type request :: Ash.Engine.Request.t()

  @type side_load :: {:side_load, Keyword.t()}
  @type prepare_instruction :: side_load
end
