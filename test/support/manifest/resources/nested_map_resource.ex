# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.NestedMapResource do
  @moduledoc """
  Test resource exercising nested map fields inside array constraints.

  Verifies that the manifest resolves nested field shapes inside an
  `{:array, :map}` attribute and preserves them in the type IR.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
  end

  actions do
    defaults [:read, :destroy]

    action :list_users_map, :map do
      constraints fields: [
                    results: [
                      type: {:array, :map},
                      constraints: [
                        items: [
                          fields: [
                            id: [type: :string],
                            email: [type: :string],
                            first_name: [type: :string, allow_nil?: true],
                            last_name: [type: :string, allow_nil?: true],
                            phone: [type: :string, allow_nil?: true],
                            is_admin: [type: :boolean, allow_nil?: true],
                            confirmed_at: [type: :utc_datetime_usec, allow_nil?: true],
                            inserted_at: [type: :utc_datetime_usec]
                          ]
                        ]
                      ]
                    ],
                    total_count: [type: :integer]
                  ]

      run fn _input, _context ->
        {:ok,
         %{
           results: [
             %{
               id: "user-1",
               email: "test@example.com",
               first_name: "Test",
               last_name: "User",
               phone: nil,
               is_admin: false,
               confirmed_at: nil,
               inserted_at: ~U[2025-01-01 00:00:00Z]
             }
           ],
           total_count: 1
         }}
      end
    end

    action :get_metrics, :map do
      constraints fields: [
                    total: [type: :integer],
                    last_week: [type: :integer],
                    last_month: [type: :integer],
                    last_year: [type: :integer]
                  ]

      run fn _input, _context ->
        {:ok,
         %{
           total: 100,
           last_week: 10,
           last_month: 40,
           last_year: 100
         }}
      end
    end

    # Deeply nested map for additional testing
    action :get_nested_stats, :map do
      constraints fields: [
                    user_stats: [
                      type: :map,
                      constraints: [
                        fields: [
                          active_users: [type: :integer],
                          new_signups: [type: :integer],
                          churn_rate: [type: :float]
                        ]
                      ]
                    ],
                    content_stats: [
                      type: :map,
                      constraints: [
                        fields: [
                          total_posts: [type: :integer],
                          posts_this_week: [type: :integer],
                          avg_engagement_rate: [type: :float]
                        ]
                      ]
                    ]
                  ]

      run fn _input, _context ->
        {:ok,
         %{
           user_stats: %{
             active_users: 1000,
             new_signups: 50,
             churn_rate: 0.05
           },
           content_stats: %{
             total_posts: 5000,
             posts_this_week: 100,
             avg_engagement_rate: 0.15
           }
         }}
      end
    end
  end
end
