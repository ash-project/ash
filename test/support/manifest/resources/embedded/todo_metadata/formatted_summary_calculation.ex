# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TodoMetadata.FormattedSummaryCalculation do
  @moduledoc """
  Formatted summary calculation for todo metadata.
  """
  use Ash.Resource.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [
      :category,
      :subcategory,
      :priority_score,
      :status,
      :is_urgent,
      :deadline,
      :tags,
      :custom_fields
    ]
  end

  @impl true
  def calculate(records, opts, _context) do
    format = opts[:format] || :short
    include_metadata = opts[:include_metadata] || false

    Enum.map(records, fn record ->
      case format do
        :short ->
          build_short_summary(record, include_metadata)

        :detailed ->
          build_detailed_summary(record, include_metadata)

        :json ->
          build_json_summary(record, include_metadata)

        _ ->
          "Unknown format"
      end
    end)
  end

  defp build_short_summary(record, include_metadata) do
    base = record.category || "Uncategorized"

    base =
      if record.subcategory do
        "#{base} > #{record.subcategory}"
      else
        base
      end

    base =
      if record.is_urgent do
        "🚨 #{base}"
      else
        base
      end

    if include_metadata do
      "#{base} (Priority: #{record.priority_score || 0})"
    else
      base
    end
  end

  defp build_detailed_summary(record, include_metadata) do
    parts = []

    # Category info
    category_part = record.category || "Uncategorized"

    category_part =
      if record.subcategory do
        "#{category_part} > #{record.subcategory}"
      else
        category_part
      end

    parts = [category_part | parts]

    # Status and urgency
    status_part = "Status: #{record.status || :draft}"

    status_part =
      if record.is_urgent do
        "#{status_part} (URGENT)"
      else
        status_part
      end

    parts = [status_part | parts]

    # Priority
    parts =
      if record.priority_score do
        ["Priority: #{record.priority_score}/100" | parts]
      else
        parts
      end

    # Deadline
    parts =
      if record.deadline do
        days_until = Date.diff(record.deadline, Date.utc_today())

        deadline_text =
          case days_until do
            days when days < 0 -> "Overdue by #{abs(days)} days"
            0 -> "Due today"
            days when days <= 7 -> "Due in #{days} days"
            _days -> "Due #{Date.to_string(record.deadline)}"
          end

        ["Deadline: #{deadline_text}" | parts]
      else
        parts
      end

    # Tags
    parts =
      if (record.tags || []) != [] do
        ["Tags: #{Enum.join(record.tags, ", ")}" | parts]
      else
        parts
      end

    # Metadata
    parts =
      if include_metadata && record.custom_fields && map_size(record.custom_fields) > 0 do
        metadata_count = map_size(record.custom_fields)
        ["Custom fields: #{metadata_count}" | parts]
      else
        parts
      end

    Enum.reverse(parts) |> Enum.join(" | ")
  end

  defp build_json_summary(record, include_metadata) do
    base_data = %{
      category: record.category,
      subcategory: record.subcategory,
      status: record.status,
      is_urgent: record.is_urgent,
      priority_score: record.priority_score
    }

    data_with_deadline =
      if record.deadline do
        Map.put(base_data, :deadline, Date.to_string(record.deadline))
      else
        base_data
      end

    data_with_tags =
      if (record.tags || []) != [] do
        Map.put(data_with_deadline, :tags, record.tags)
      else
        data_with_deadline
      end

    final_data =
      if include_metadata && record.custom_fields && map_size(record.custom_fields) > 0 do
        Map.put(data_with_tags, :custom_fields, record.custom_fields)
      else
        data_with_tags
      end

    Jason.encode!(final_data)
  rescue
    _ -> "{\"error\": \"Failed to encode JSON\"}"
  end
end
