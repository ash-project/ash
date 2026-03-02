# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.EctoCompat do
  @moduledoc """
  Introspects Ash resources for Ecto-compat issues.

  ## Background

  Ash resources automatically generate an Ecto schema under the hood, but certain
  features don't translate 1:1 between Ash and Ecto. Specifically:

    * **Timestamps**: Ash's `create_timestamp` and `update_timestamp` use Ash's own
      change pipeline to populate values, but Ecto's `timestamps()` macro registers
      fields via `__schema__(:autogenerate_fields)` so that `Repo.insert/1` and
      `Repo.update/1` can fill them in automatically. Ash-generated schemas do NOT
      set these autogenerate entries, so calling `Repo.insert/1` directly on an
      Ash resource struct will leave timestamp fields as `nil`.

    * **Static defaults**: Ash attributes can declare `default <value>`, but this
      default is applied by the Ash changeset pipeline — it is NOT baked into the
      Ecto struct definition. So `%MyResource{}` will show `nil` for those fields
      instead of the Ash default.

  This module provides functions to detect both issues and report them as warnings.

  See: https://github.com/ash-project/ash/issues/769
  """

  alias Ash.Resource.Info

  @doc """
  Inspect a single Ash resource module and return a list of warning maps.

  Returns `[]` if there are no compatibility issues, or a list of maps each
  containing a `:type` key (`:missing_autogenerate`, `:default_mismatch`, or
  `:error`) along with descriptive fields.

  We use a `cond` for early-return semantics: if the module isn't loaded or
  isn't a valid Ash resource, we return an error immediately without attempting
  to call `Info.attributes/1` (which would raise an `ArgumentError` from Spark
  for non-DSL modules).
  """
  @spec inspect_resource(module()) :: [map()]
  def inspect_resource(resource) when is_atom(resource) do
    cond do
      # Guard: make sure the module is compiled and loaded into the VM.
      # An atom like `UnloadedModule12345` won't be loaded — we catch that here
      # instead of letting downstream calls blow up.
      not Code.ensure_loaded?(resource) ->
        return_error("Resource module #{inspect(resource)} is not loaded")

      # Guard: the module exists but isn't an Ash resource (e.g. `String`).
      # `Info.resource?/1` delegates to `Spark.Dsl.is?/2` which is safe for
      # any module — it returns false rather than raising.
      not Info.resource?(resource) ->
        return_error("#{inspect(resource)} is not an Ash resource")

      # Both guards passed — this is a valid, loaded Ash resource. Run the
      # actual compatibility checks.
      true ->
        do_inspect_resource(resource)
    end
  end

  # ── Core inspection logic ─────────────────────────────────────────────

  # Runs the two compatibility checks against a confirmed Ash resource.
  # We gather data from both the Ash introspection API and the generated
  # Ecto schema, then compare them.
  defp do_inspect_resource(resource) do
    # Ask Ecto's generated schema which fields it auto-generates.
    # For a normal `timestamps()` call this would return [:inserted_at, :updated_at].
    # For Ash-generated schemas this is typically [] — that's the core problem.
    ecto_autogen = ecto_autogenerate_fields(resource)

    # Ask Ash's introspection API for the full list of declared attributes
    # on this resource (primary keys, regular attributes, timestamps, etc.).
    ash_attrs = Info.attributes(resource)

    # Filter down to just the timestamp-like attributes. We identify these
    # heuristically by checking type, writability, default function, etc.
    # (see `timestamp_attr?/1` below for the full heuristic).
    ash_timestamp_names =
      ash_attrs
      |> Enum.filter(&timestamp_attr?/1)
      |> Enum.map(& &1.name)

    # Filter to attributes that have a static (non-function) Ash default.
    # These are the ones that might not show up in `%Resource{}`.
    ash_default_attrs =
      ash_attrs
      |> Enum.filter(&has_ash_default?/1)

    # Build a map of {field_name => default_value} from the bare struct.
    # For most Ash fields this will be `nil` since Ash doesn't set struct defaults.
    struct_defaults = get_struct_defaults(resource)

    # Start with an empty warnings list and pipe through each check.
    # Each check either appends warnings or passes the list through unchanged.
    []
    |> check_missing_autogen_timestamps(resource, ash_timestamp_names, ecto_autogen)
    |> check_default_mismatches(resource, ash_default_attrs, struct_defaults)
  end

  # ── Helpers ────────────────────────────────────────────────────────────

  # Wraps an error message in the standard warning-map format so callers
  # can handle errors the same way they handle warnings.
  defp return_error(message) do
    [%{type: :error, message: message}]
  end

  # Safely retrieves the list of auto-generated fields from the Ecto schema.
  #
  # Ecto schemas define `__schema__(:autogenerate_fields)` which returns the
  # list of fields that Ecto will auto-populate on insert/update (e.g. timestamps).
  # We wrap this in try/rescue because:
  #   1. The module might not define `__schema__/1` at all (non-Ecto module).
  #   2. Even if it does, calling it could theoretically raise in edge cases.
  defp ecto_autogenerate_fields(resource) do
    if function_exported?(resource, :__schema__, 1) do
      try do
        resource.__schema__(:autogenerate_fields) || []
      rescue
        _ -> []
      catch
        _ -> []
      end
    else
      []
    end
  end

  # Builds a map of field names to their default values from the bare struct.
  #
  # We call `struct(resource)` to get `%Resource{field: default, ...}` and then
  # convert it to a plain map. This tells us what values Ecto/Elixir would use
  # if you did `Repo.insert(%Resource{})` — fields not set in the struct
  # definition will be `nil`.
  defp get_struct_defaults(resource) do
    try do
      struct(resource) |> Map.from_struct()
    rescue
      _ -> %{}
    catch
      _ -> %{}
    end
  end

  # ── Timestamp detection ────────────────────────────────────────────────

  # Determines if an Ash attribute looks like a timestamp created by
  # `create_timestamp` or `update_timestamp` in the DSL.
  #
  # Ash doesn't tag these attributes with a special "I'm a timestamp" flag,
  # so we use a heuristic based on the combination of properties that
  # timestamp attributes always have:
  #
  #   - Type is a datetime variant (utc_datetime, naive_datetime, etc.)
  #   - Not directly writable by users (writable? == false)
  #   - Has a function or MFA tuple as its default (e.g. &DateTime.utc_now/0)
  #   - match_other_defaults? is true (ensures all timestamps in a changeset
  #     share the same value)
  #   - Cannot be nil (allow_nil? == false)
  #   - For update_timestamp: also has an update_default (function or MFA)
  #   - For create_timestamp: update_default is nil
  defp timestamp_attr?(attr) do
    is_datetime_type?(attr.type) &&
      attr.writable? == false &&
      (is_function(attr.default) || match?({_, _, _}, attr.default)) &&
      attr.match_other_defaults? == true &&
      attr.allow_nil? == false &&
      (is_nil(attr.update_default) ||
         is_function(attr.update_default) || match?({_, _, _}, attr.update_default))
  end

  # Checks if a type is one of the known datetime types, covering both
  # Ash's custom type modules and Ecto's built-in atom types.
  defp is_datetime_type?(type) do
    type in [
      # Ash's own type wrappers
      Ash.Type.UtcDatetime,
      Ash.Type.UtcDatetimeUsec,
      Ash.Type.NaiveDatetime,
      Ash.Type.NaiveDatetimeUsec,
      # Ecto's built-in type atoms (in case someone uses these directly)
      :utc_datetime,
      :utc_datetime_usec,
      :naive_datetime,
      :naive_datetime_usec
    ]
  end

  # ── Default detection ──────────────────────────────────────────────────

  # Returns true if the attribute has a *static* (literal) Ash default.
  #
  # We exclude function defaults and MFA tuples because those are dynamic —
  # they're computed at insert time by the Ash changeset pipeline. We only
  # care about static values like `default 1` or `default "active"` because
  # those are the ones users might expect to see on `%Resource{}` but won't.
  defp has_ash_default?(attr) do
    not is_nil(attr.default) &&
      not (is_function(attr.default) || match?({_, _, _}, attr.default))
  end

  # ── Check: missing autogenerate timestamps ─────────────────────────────

  # Compares the Ash timestamp field names against the Ecto autogenerate list.
  #
  # If Ash has timestamp fields (e.g. :inserted_at, :updated_at) that are NOT
  # in Ecto's autogenerate list, those fields won't be auto-populated when
  # using `Repo.insert/1` or `Repo.update/1` directly. This is the core bug
  # described in https://github.com/ash-project/ash/issues/769.
  #
  # We prepend any warnings to the accumulator list and pass it along.
  defp check_missing_autogen_timestamps(warnings, resource, ash_timestamps, ecto_autogen) do
    # Find timestamp fields that Ash knows about but Ecto doesn't auto-generate.
    missing =
      ash_timestamps
      |> Enum.reject(&(&1 in ecto_autogen))

    if missing == [] do
      # No gap — all Ash timestamps are wired up in Ecto. Pass warnings through.
      warnings
    else
      [
        %{
          type: :missing_autogenerate,
          resource: resource,
          fields: missing,
          message:
            "Ash timestamps #{inspect(missing)} exist but are not in " <>
              "#{inspect(resource)}.__schema__(:autogenerate_fields) = #{inspect(ecto_autogen)}"
        }
        | warnings
      ]
    end
  end

  # ── Check: default mismatches ──────────────────────────────────────────

  # Compares Ash's static default values against what the bare Ecto struct has.
  #
  # For each Ash attribute with a static default, we check whether the struct
  # also has that default. If the struct shows `nil` but Ash has a default,
  # that means `%Resource{}` and `Repo.insert(%Resource{})` won't include the
  # default — only `Ash.create/2` will.
  defp check_default_mismatches(warnings, resource, ash_default_attrs, struct_defaults) do
    mismatches =
      Enum.flat_map(ash_default_attrs, fn attr ->
        # Look up what the bare struct has for this field.
        struct_default = Map.get(struct_defaults, attr.name)

        # If the struct shows nil but Ash has a non-nil default, that's a mismatch.
        if struct_default == nil and attr.default != nil do
          [%{attr: attr.name, ash_default: attr.default, struct_default: struct_default}]
        else
          []
        end
      end)

    if mismatches == [] do
      # No mismatches found — pass warnings through unchanged.
      warnings
    else
      [
        %{
          type: :default_mismatch,
          resource: resource,
          details: mismatches,
          message:
            "Ash defaults are not visible on the bare struct of #{inspect(resource)}. " <>
              "Using Repo.insert/1 may differ from Ash.create/2."
        }
        | warnings
      ]
    end
  end

  # ── Output formatting ─────────────────────────────────────────────────

  @doc """
  Print a list of warning maps in a human-readable, colour-coded format.

  Each warning type gets a different emoji prefix:
    - :error           -> red X
    - :missing_autogenerate -> red circle (critical — timestamps won't work)
    - :default_mismatch     -> yellow circle (advisory — defaults differ)

  Returns `:ok` so callers can assert on the return value in tests.
  """
  @spec print_warnings([map()]) :: :ok
  def print_warnings(warnings) when is_list(warnings) do
    if warnings == [] do
      IO.puts("✅ No Ecto compatibility issues found!")
    else
      IO.puts("\n⚠️  Ecto Compatibility Warnings:\n")

      # Pattern-match each warning type and format accordingly.
      Enum.each(warnings, fn
        %{type: :error, message: msg} ->
          IO.puts("❌ ERROR: #{msg}\n")

        %{type: :missing_autogenerate, resource: resource, fields: fields, message: msg} ->
          IO.puts("🔴 Missing Autogenerate Fields")
          IO.puts("   Resource: #{inspect(resource)}")
          IO.puts("   Fields: #{inspect(fields)}")
          IO.puts("   Details: #{msg}\n")

        %{type: :default_mismatch, resource: resource, details: details, message: msg} ->
          IO.puts("🟡 Default Mismatch")
          IO.puts("   Resource: #{inspect(resource)}")
          IO.puts("   Details: #{msg}")

          # List each individual field that has a default mismatch.
          Enum.each(details, fn %{attr: name, ash_default: ash_default} ->
            IO.puts("     * #{name}: Ash default = #{inspect(ash_default)}, struct default = nil")
          end)

          IO.puts("")
      end)
    end
  end
end
