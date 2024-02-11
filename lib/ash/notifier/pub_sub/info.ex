defmodule Ash.Notifier.PubSub.Info do
  @moduledoc "Introspection helpers for Ash.Notifier.PubSub"

  @doc "The list of publications for a resource"
  def publications(resource) do
    Spark.Dsl.Extension.get_entities(resource, [:pub_sub])
  end

  @doc "The pubsub module for a resource"
  def module(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:pub_sub], :module, nil)
  end

  @doc "The topic prefix for a resource"
  def prefix(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:pub_sub], :prefix, nil)
  end

  @doc "The delimiter to use when generating message topics"
  def delimiter(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:pub_sub], :delimiter, ":")
  end

  @doc "The pubsub name for a resource"
  def name(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:pub_sub], :name, nil)
  end

  @doc "The broadcast type for a resource"
  def broadcast_type(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:pub_sub], :broadcast_type, nil)
  end
end
