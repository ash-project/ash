# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Temporal.Gated do
  @moduledoc """
  A policy-protected resource whose read policy is a `now()`-based filter check.

  Used to prove that `as_of` flows into authorization (so `now()` in a policy is
  anchored to the read's instant, not the wall clock) and that policy early/eager
  evaluation does not resolve a relative-time check against the wall clock.

  Not declared `temporal` (no data layer here supports it) — `as_of` is a core
  field that anchoring keys off whenever it is set, so passing it explicitly is
  enough to exercise the authorization path.
  """
  use Ash.Resource,
    domain: Ash.Test.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private? true
  end

  # Allowed only "before" 2026-04-01, evaluated at the read's `as_of`.
  @cutoff ~U[2026-04-01 00:00:00.000000Z]

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  policies do
    policy action_type(:read) do
      authorize_if expr(now() < ^@cutoff)
    end
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string, public?: true
  end
end
