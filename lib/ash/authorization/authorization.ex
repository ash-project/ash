defmodule Ash.Authorization do
  @moduledoc """
  #TODO: Explain authorization

  Authorization in Ash is done via declaring `authorization_steps` for actions,
  and in the case of stateful actions, via declaring `authoriation_steps` on attributes
  and relationships.

  In the case of `read` actions
  """

  @type request :: Ash.Authorization.Request.t()

  # Required sideloads before checks are run
  # @type side_load_instruction :: {:side_load, Ash.side_load()}
  # The result for this check is predetermined for all records
  # that could be passed in from this request.
  @type decision :: {:decision, boolean}
  # @type precheck_context :: {:context, %{optional(atom) => term}}
  # @type precheck_result :: side_load_instruction() | decision() | precheck_context()
  @type precheck_result :: decision()
end
