defmodule Mix.Tasks.Ash.Gen.Resource do
  @shortdoc "Generates all Ash resources from a postgres database"

  # ruby on rails code for getting most of the info we'd need from a PG database to create all the resources


  # con = PG.connect :dbname => "#{DATABASE_NAME}"

  # tables = []

  # tables = con.exec("
  #   select table_name, table_type
  #   from information_schema.tables
  #   WHERE table_schema='public'
  #   AND NOT table_name='schema_migrations'
  #   AND NOT table_name='ar_internal_metadata'
  # ").to_a

  # columns = con.exec("
  #   SELECT
  #     columns.column_name as name,
  #     columns.column_default as default,
  #     columns.is_nullable as allow_nil,
  #     columns.udt_name as type,
  #     columns.table_name,
  #     constraints.constraint_type as constraint
  #   FROM (
  #     select column_name, column_default, is_nullable, udt_name, table_name
  #     from information_schema.columns
  #     WHERE table_schema='public'
  #     AND NOT table_name='schema_migrations'
  #     AND NOT table_name='ar_internal_metadata'
  #   ) columns
  #   LEFT JOIN
  #     (SELECT c.column_name, c.table_name, t.constraint_type
  #     FROM information_schema.key_column_usage AS c
  #     LEFT JOIN information_schema.table_constraints AS t
  #     ON t.constraint_name = c.constraint_name
  #     WHERE t.table_name = c.table_name
  #     AND c.table_schema='public'
  #     AND NOT c.table_name='schema_migrations'
  #     AND NOT c.table_name='ar_internal_metadata'
  #   ) constraints
  #   ON columns.table_name = constraints.table_name
  #   AND columns.column_name = constraints.column_name
  # ")

  # schema = []

  # tables.each do |table|
  #   schema << {
  #     name: table["table_name"],
  #     type: table["table_type"],
  #     columns: columns.filter{|c| c["table_name"] == table["table_name"] }
  #   }
  # end
end