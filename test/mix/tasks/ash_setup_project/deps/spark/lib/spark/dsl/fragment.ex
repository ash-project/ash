# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Fragment do
  @moduledoc """
  Allows splitting up a DSL into multiple modules, potentially organizing large DSLs

  Use the `of` option to expression what your fragment is a fragment of. You can add
  extensions as you would normally to that resource, and they will be added to the
  parent resource.

      defmodule MyApp.Resource.Graphql do
        use Spark.Dsl.Fragment, of: Ash.Resource, extensions: AshGraphql.Resource

        graphql do
          ...
        end
      end

  Then add the fragment to the parent resource.

      defmodule MyApp.Resource do
        use Ash.Resource, fragments: [MyApp.Resource.Graphql], ...
      end
  """

  defmacro __using__(opts) do
    opts = Spark.Dsl.Extension.do_expand(opts, __CALLER__)
    original_opts = opts
    single_extension_kinds = opts[:of].single_extension_kinds()
    many_extension_kinds = opts[:of].many_extension_kinds()

    {opts, extensions} =
      opts[:of].default_extension_kinds()
      |> Enum.reduce(opts, fn {key, defaults}, opts ->
        Keyword.update(opts, key, defaults, fn current_value ->
          cond do
            key in single_extension_kinds ->
              current_value || defaults

            key in many_extension_kinds || key == :extensions ->
              List.wrap(current_value) ++ List.wrap(defaults)

            true ->
              current_value
          end
        end)
      end)
      |> Spark.Dsl.expand_modules(
        [
          single_extension_kinds: single_extension_kinds,
          many_extension_kinds: many_extension_kinds
        ],
        __CALLER__
      )

    extensions =
      extensions
      |> Enum.flat_map(&[&1 | &1.add_extensions()])
      |> Enum.uniq()

    Module.register_attribute(__CALLER__.module, :spark_extension_kinds, persist: true)
    Module.register_attribute(__CALLER__.module, :spark_fragment_of, persist: true)

    Module.put_attribute(__CALLER__.module, :spark_fragment_of, opts[:of])
    Module.put_attribute(__CALLER__.module, :extensions, extensions)
    Module.put_attribute(__CALLER__.module, :original_opts, original_opts)

    Module.put_attribute(
      __CALLER__.module,
      :spark_extension_kinds,
      List.wrap(many_extension_kinds) ++
        List.wrap(single_extension_kinds)
    )

    quote do
      require unquote(opts[:of])
      unquote(Spark.Dsl.Extension.prepare(extensions))
      @before_compile Spark.Dsl.Fragment
    end
  end

  defmacro __before_compile__(_) do
    quote do
      Spark.Dsl.Extension.set_state([], [], false)

      def extensions do
        @extensions
      end

      def opts do
        @original_opts
      end

      def spark_dsl_config do
        @spark_dsl_config
      end

      def validate_sections do
        List.wrap(@validate_sections)
      end

      @persisted @spark_dsl_config[:persist]

      def persisted do
        @persisted
      end
    end
  end
end
