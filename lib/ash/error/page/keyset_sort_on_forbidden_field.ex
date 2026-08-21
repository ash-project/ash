# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Error.Page.KeysetSortOnForbiddenField do
  @moduledoc """
  Used when a keyset-paginated read is sorted on a field a field policy forbids
  the current actor from reading.

  The field would be redacted on the returned rows, so the keyset cursor could
  only be built from the redaction marker rather than the sort value, silently
  breaking pagination. The read is rejected instead.

  If the sort comes from user input, use `Ash.Query.sort_input/2`, which takes
  field policies into account. Otherwise, avoid sorting on a field this actor
  cannot read while using keyset pagination.
  """

  use Splode.Error, fields: [:resource, :field], class: :invalid

  def message(%{resource: resource, field: field}) do
    """
    Cannot keyset-paginate #{inspect(resource)} sorted on #{inspect(field)}: a field policy forbids the current actor from reading #{inspect(field)}.

    The field would be redacted on the returned rows, so the keyset cursor could only be built from the redaction marker rather than the sort value, which silently breaks pagination.

    If this sort comes from user input, use `Ash.Query.sort_input/2`. Otherwise, don't sort on #{inspect(field)} with keyset pagination for an actor that cannot read it.
    """
  end
end
