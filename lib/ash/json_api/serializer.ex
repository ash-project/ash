defmodule Ash.JsonApi.Serializer do
  alias Ash.Request

  def serialize_one(request, record) do
    # TODO `links` and `included`
    data = serialize_one_record(request, record)
    json_api = %{version: "1.0"}

    Jason.encode!(%{data: data, json_api: json_api})
  end

  defp serialize_one_record(%Request{resource: resource}, record) do
    # TODO: `attributes` `relationships` `links` `meta`
    %{
      id: record.id,
      type: Ash.type(resource)
    }
  end
end
