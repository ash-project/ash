# What is Ash?

At a high level, Ash is an opinionated, foundational set of application building blocks, designed for maximum compatibility, reuse and extensibility. It can be used for any kind of Elixir application, but it shines when building web applications, APIs and services. It is not a web framework, like Phoenix or Rails. It is a framework for building your application layer, independent of how it is exposed or consumed. It is not an **alternative** to frameworks like Phoenix, rather a **complement** to them.

More concretely, Ash provides a "Resource" abstraction, with which you model your application. These resources determine the database schema, API endpoints, state machines, background jobs, and more. They are the source of truth for your entire application, and everything stems from them.

Ash is declarative-first, with deep extensibility. We provide a suite of extensions, as well as a toolkit to build your own. When you need to break out of our design patterns, we provide escape hatches ranging from the simple and small in scope to abilities to override large pieces of behavior. Additionally, your Ash application is just an Elixir application, so if you want to do something completley custom outside of Ash, it won't get in your way.

> #### Model your domain, derive the rest {: .info}
> With the tools provided by Ash you can get entire application layers, derived and configured directly by your resources, practically for free.
> Ash leverages the best of the Elixir ecosystem under the hood, but provides a single unified tool-chain for our users.

---

A good analogy is design systems in the world of web development. When you use a design system, you get a set of components that are designed to work together, and you can build your application by combining these components in different ways. Ash is like a design system for your application's domain model.

Or, for a less technical analogy, Ash is like a fully stocked workshop. When you arrive at the workshop, you may need to learn where everything is, but once you do, you have everything you need to build anything you can dream up.

## Why should I use it?

The fundamental idea behind Ash is that when the various components of your system can have consistent expectations of how the other components around them work, you can ultimately do a significant amount more, with less. For example, once you've defined your resources, it takes only a few additional lines of code to have your database structure generated, and a full featured API built around it.

Ash has many use cases, with varying degrees of complexity. Ash helps you on day 1 of your project, removing boiler plate and allowing you to focus on the essential complexity of your application. It also helps you on year 5, lending consistency, code reuse, and maintainability.

> #### Ash gives you super powers {: .tip}
> Things that once took days or weeks can be done in hours, and to a degree of quality that would have been unreasonable before. Entire classes of bugs are eliminated. And the best part is, you can do all of this without sacrificing the flexibility, robustness and ecosystem that Elixir is known for.

## Watch the ElixirConf 2023 Talk

<iframe width="560" height="315" style="aspect-ratio: 16 / 9; border-radius: 0.75rem;" src="https://www.youtube.com/embed/c4iou77kOFc?si=gxPdzGng5cQTrr7P" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen />
