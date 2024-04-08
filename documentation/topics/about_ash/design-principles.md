# Design Principles

The design principles behind Ash allows us to build an extremely flexible and powerful set of tools, without locking users into specific choices at any level. The framework acts as a spinal cord for your application, with extension points at every level to allow for custom behavior. What follows are the core tenets behind Ash Framework.

## Anything, not Everything

"Anything, not Everything" means building a framework capable of doing anything, not providing a framework that already does everything. The first is possible, the second is not. Our primary goal is to provide a framework that _unlocks_ potential, and frees developers to work on the things that make their application special.

To this end, there are many prebuilt extensions to use, but there is also a rich suite of tools to build your _own_ extensions. In this way, you can make the framework work for you, instead of struggling to fit your application to a strictly prescribed pattern. Use as much of Ash as you can, and leverage the amazing Elixir ecosystem for everything else.

## Declarative, Introspectable, Derivable

The real superpower behind Ash is the declarative design pattern. All behavior is driven by explicit, static declarations. A resource, for example, is really just a configuration file. On its own it does nothing. It is provided to code that reads that configuration and acts accordingly.

You can read more about some simple declarative design patterns outside of the context of Ash Framework in [An Incremental Approach to Declarative Design](https://zachdaniel.dev/incremental-declarative-design).

## Configuration over Convention

While convention has value, we believe that explicit configuration ultimately leads to more discoverable, maintainable and flexible applications than a convention driven approach. This means that we never do things like assume that files with a given name are a certain type of thing, or that because a file is in a certain location, it should perform a specific function.

## Pragmatism First

While Ash does have lofty goals and a roadmap, the priority for development is always what the _current_ users of Ash need or are having trouble with. We focus on simple, pragmatic, and integrated solutions that meld well with the rest of the framework.

A high priority is placed on ensuring that our users don't experience feature whip-lash due to poorly thought out implementations, and that any breaking changes (a rare occurrence) have a clean and simple upgrade path. This is something made much easier by the declarative pattern.

## Community

The Ash community comes together and collaborates to make sure that we can all build our software quickly, effectively and in a way that will age gracefully. We have a strict code of conduct, and love working with people of any experience level or background. To experience this first-hand, participate on [ElixirForum](https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum/123) or join our [discord](https://discord.gg/D7FNG2q)!

> ### Domain Driven Design? {: .info}
>
>  Ash is _not_ a Domain Driven Design framework, if we're talking about "proper" Domain Driven Design as it is taught and discussed today. Domain Driven Design comes with a considerable amount of baggage and unnecessary complexity. While we identify with the *goals* of Domain Driven Design, we believe that a simpler approach is more effective, and that much of what DDD teaches are actually _implementation_ details, and not _design_ concepts. If the name wasn't taken, we would surely have claimed it for ourselves. If you must have a similar term for Ash, consider it a "Resource-oriented, Declarative Design Application Framework".
