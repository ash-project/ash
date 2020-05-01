# defmodule Ash.Filter.Error do
#   defexception [:errors]

#   def exception(opts) do
#     engine = opts[:filter]

#     %__MODULE__{errors: filter.errors}
#   end

#   def message(error) do
#     header = "Authorized?: #{error.authorized?}\n\n"

#     body =
#       Enum.map_join(error.errors, fn {path, value} ->
#         "path: #{inspect(path)}\n" <> Ash.ErrorKind.message(value)
#       end)

#     header <> body
#   end
# end
