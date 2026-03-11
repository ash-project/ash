# AALang and GAB

## Why AALang?

**AALang is the first programming language designed to run entirely within the LLM.** Unlike traditional prompt engineering approaches that rely on ad-hoc instructions and unstructured conversations, AALang provides a formal, graph-native language specification that LLMs execute with **greater consistency and fewer hallucinations**.

**The Problem with Standard Prompt Engineering:**
- Inconsistent behavior across sessions
- Hallucinations and unpredictable outputs
- Difficult to maintain and version control
- No formal structure for complex agent behaviors
- Hard to debug and reason about

**The AALang Solution:**
- ✅ **Formal Language Specification**: JSON-LD graph format provides structure and consistency
- ✅ **Reduced Hallucinations**: Explicit mode constraints and actor responsibilities guide LLM behavior
- ✅ **Version Control**: Language specifications are code, not prompts - track changes, review diffs, collaborate
- ✅ **Reproducible Results**: Same specification produces consistently bounded behavior across different LLM instances
- ✅ **Built for LLMs**: Designed from the ground up for LLM execution, not adapted from human-readable languages
- ✅ **Graph-Native**: Leverages LLMs' natural ability to understand graph structures and relationships

**Build production-ready LLM agents with the consistency of code and the flexibility of natural language.**

**GAB** (Generic AALang Builder) is an intelligent agent compiler that helps you build AALang-based products including tools, games, agents, protocols, and more. GAB uses a structured 4-mode workflow to guide you through the entire development process, from initial concept to final product compiled to AALang.

**Is GAB really a compiler?** See the [comprehensive argument](README_support_docs/is-gab-a-compiler.md) addressing this question.

## What is AALang?

**AALang** (Actor-based Agent Language) is a programming language designed specifically for LLM agent consumption and execution. It's optimized for graph-native, LLM-friendly development and supports bounded non-determinism. AALang is **MCP (Model Context Protocol) and A2A (Agent to Agent) ready**, making it perfect for integration with modern LLM tooling and distributed agent systems.

**Is AALang Turing complete?** See the [arguments and analysis](README_support_docs/turing-complete.md).

**Is AALang truly concurrent, or just pretending?** Explore the [concurrency and parallelism analysis](README_support_docs/concurrent-parallel.md).

**Is AALang really a programming language?** Read the [comprehensive argument](README_support_docs/is-aalang-a-language.md) addressing this question.

Learn more at: **[https://aalang.org](https://aalang.org)**

## Tested On

**Generation** of products using GAB and tested using AATest has been tested.

Works on:
- Cursor (In Agent Mode)
  - Auto


**Execution** capability testing has been conducted using the [babylon 5 fan game](https://github.com/yenrab/fab-fan), which provides a medium to medium-high level of complexity for evaluating AALang capabilities.

Works on:
- Ollama 
  - GUI use @context
  - HTTP server use system prompt
- LM Studio
  - GUI use system prompt
  -HTTP server use system prompt
- Cursor (In Agent Mode)
  - Auto
  - composer1 
  - claude-4.5-sonnet-thinking 
  - grok-code-fast-1 
  - gemini-3-pro-preview

**Note** Stateful AALang tools created by GAB need significant context windows to not loose the instructions and states. Cursor's summaries appear to retain the behaviors and states of AALang tools. Small models, 4b, quickly run out of context window space and loose the tool.

## Actors and Personas

**Important**: You don't need to build actors and personas manually. **GAB automatically builds them for you** based on your product description. This section explains what they are and how they work so you can understand what GAB creates and how to effectively describe your product requirements.

Understanding the relationship between **actors** and **personas** helps you work effectively with GAB and understand how your generated AALang products function.

### Actors: The Core Reasoning Units

**Actors** are individual reasoning units within an LLM agent. They are the primary building blocks of AALang's n-mode-m-actor architecture:

- **All actors are stateful** - Each actor maintains its own isolated context and state
- **Actors operate in modes** - They can switch between different behavioral modes with distinct constraints
- **Actors communicate via message passing** - They interact with other actors (same agent or different agents) through structured messages
- **Actors are required** - Every AALang agent must have at least one actor

### Personas: Optional Internal Reasoning Patterns

**Personas** are optional library patterns that actors can employ for internal deliberation:

- **Personas are contained within actors** - They exist as internal reasoning patterns, not as separate entities
- **Actors choose when to use personas** - Personas are employed selectively when an actor needs structured internal deliberation
- **Personas enable multi-perspective reasoning** - Multiple personas within an actor can debate, negotiate, and reach consensus before the actor takes action
- **Personas are optional** - An actor can function perfectly well without personas

### The Relationship

Think of the relationship this way:

- **Actor = The decision-maker** - The actor is the entity that takes actions, maintains state, and operates in modes
- **Personas = Internal advisors** - Personas are like a "board of advisors" that the actor can consult internally before making decisions

### Important Implementation Consideration

**Note on Practical Implementation Patterns:**

While the AALang specification states that personas are optional, in practice, many implementations use a pattern where:

- **Actors are stateful containers** - Actors maintain state (`stateful: true`) and operate in modes, but may not have `responsibilities`, `canMessage`, or `canReceiveFrom` properties directly defined
- **Personas provide actionable capabilities** - Personas define the `responsibilities` (what tasks can be accomplished), `canMessage`/`canReceiveFrom` (communication capabilities), and `sessionConsistent` behavior
- **Actors without personas may be non-functional** - In this implementation pattern, actors without personas cannot communicate or accomplish tasks because they lack the necessary capability definitions

This is a **practical implementation choice**, not a requirement of the AALang specification. The specification allows actors to have responsibilities and communication capabilities directly, or to delegate them to personas. When using GAB or examining existing AALang products, you may find that actors delegate their capabilities to personas, making personas effectively required for functionality even though they're theoretically optional.

**Key Distinction:**
- **Actor statefulness** (`stateful: true`) - The actor maintains its own isolated context and state
- **Persona session consistency** (`sessionConsistent: true`) - The persona maintains consistent behavior and capabilities across sessions

**GAB automatically builds actors and personas** based on your product description. You describe what you want to build, and GAB creates the appropriate actors and personas with the right capabilities, responsibilities, and communication patterns.

### When to Use Personas

Personas are particularly useful when:

- **Complex decisions require deliberation** - The actor needs to consider multiple perspectives before acting
- **Structured negotiation is needed** - Different viewpoints need to be weighed and reconciled
- **Bounded non-determinism is desired** - Persona-based variance allows for different reasoning paths while staying within acceptable bounds
- **Internal consensus is important** - The actor should deliberate internally before external actions
- **Capability delegation pattern** - When implementing a pattern where actors delegate responsibilities and communication to personas (common in GAB-generated products)

### Communication Layers

AALang uses a three-layer communication architecture:

- **Layer 0**: Agent-to-Agent (gossip-based P2P)
- **Layer 1**: Actor-to-Actor (local graph routing within same agent)
- **Layer 2**: Persona-to-Persona (internal reasoning within same actor)

Personas communicate at Layer 2, which is the most internal layer. When personas from different actors need to communicate, the communication flows through the actor layer (Layer 1) and potentially the agent layer (Layer 0).

### How GAB Builds Actors and Personas

When you describe your product to GAB, it automatically:

1. **Creates actors** - GAB analyzes your requirements and creates the necessary actors with appropriate modes and state management
2. **Creates personas** - GAB generates personas with specific responsibilities, communication capabilities, and behavioral characteristics based on your product needs
3. **Establishes communication patterns** - GAB sets up communication matrices (`canMessage`/`canReceiveFrom`) between personas based on workflow requirements
4. **Assigns responsibilities** - GAB distributes tasks and capabilities across personas based on your product description
5. **Configures state management** - GAB sets up actor statefulness and persona session consistency as needed

**What you need to do**: Simply describe your product idea clearly. GAB handles all the technical details of creating actors, personas, modes, and communication patterns.

**What this means for you**: Understanding actors and personas helps you:
- Describe your product requirements more effectively to GAB
- Understand the generated AALang product structure
- Debug and refine your product if needed
- Appreciate how your product works under the hood

**Remember**: Actors are the fundamental units of computation in AALang. Personas are patterns that enhance an actor's capabilities. GAB automatically creates both based on your product description - you don't need to design them manually.

## What Can You Build with GAB?

**AALang is a general-purpose programming language** - you can build virtually anything that can be expressed computationally. GAB helps you create:

- **🎮 Games** - Interactive games powered by LLM agents
- **🛠️ Tools** - Utilities and applications that leverage LLM capabilities
- **🤖 Agents** - Custom LLM agents with specific behaviors and modes
- **📋 Protocols** - Communication and interaction protocols
- **💬 Communication Patterns** - Patterns for agent-to-agent or agent-to-user communication
- **📦 Any AALang-based Product** - Anything that conforms to AALang specifications

## Best Practices

Follow these best practices guides to build stable, production-ready GAB products that require fewer modifications and bug fixes:

- **🎮 [Game Creation Best Practices](README_support_docs/game-creation-best-practices.md)** 🚧 - Best practices for creating interactive games with GAB
- **🛠️ [Tool Creation Best Practices](README_support_docs/tool-creation-best-practices.md)** 🚧 - Best practices for building tools and utilities with GAB
- **🤖 [Agent Creation Best Practices](README_support_docs/agent-creation-best-practices.md)** - Best practices for creating custom LLM agents with GAB
- **📋 [Protocol Creation Best Practices](README_support_docs/protocol-creation-best-practices.md)** 🚧 - Best practices for designing communication and interaction protocols
- **💬 [Communication Pattern Creation Best Practices](README_support_docs/communication-pattern-creation-best-practices.md)** 🚧 - Best practices for creating agent-to-agent and agent-to-user communication patterns
- **📦 [AALang Product Creation Best Practices](README_support_docs/aalang-product-creation-best-practices.md)** 🚧 - General best practices for any AALang-based product

## Learn More

- **AALang Website**: [https://aalang.org](https://aalang.org)
- **Turing Completeness Analysis**: [Is AALang probabilistically Turing complete?](README_support_docs/turing-complete.md) - Deep dive into AALang's computational capabilities
- **Concurrency and Parallelism Analysis**: [Is AALang truly concurrent, or just pretending?](README_support_docs/concurrent-parallel.md) - Examination of AALang's concurrent/parallel architecture
- **Language Status**: [Is AALang really a programming language?](README_support_docs/is-aalang-a-language.md) - Comprehensive argument addressing language classification
- **Compiler Status**: [Is GAB really a compiler?](README_support_docs/is-gab-a-compiler.md) - Comprehensive argument addressing compiler classification
- **Documentation**: See the specification files for detailed technical information

## How GAB Works

GAB uses a **4-mode-13-actor** pattern with a structured workflow:

### 1. **Clarification Mode** 🔍
- Analyzes your initial product description
- Identifies ambiguities and missing information
- Asks targeted questions to understand requirements
- Calculates understanding confidence scores
- Ensures you have a clear vision before proceeding

### 2. **Discussion Mode** 💬
- Decomposes the problem into manageable components
- Designs the architecture (modes, actors, personas)
- Proposes solutions and alternatives
- Facilitates consensus between personas
- Creates the initial design specification

### 3. **Formalization Mode** ✅
- Analyzes the design for logic errors and inconsistencies
- Verifies AALang design compliance
- Checks for common bugs and edge cases
- Ensures quality and completeness
- Finalizes the specification

### 4. **Generation Mode** 🚀
- Creates the final AALang product files
- Generates JSON-LD formatted specifications
- Implements all designed components
- Produces ready-to-use AALang code

**After Generation**: Once GAB generates your initial product, follow the [GAB Development Workflow](README_support_docs/gab-development-workflow.md) to refine and test your product. This workflow guides you through actor self-checks, non-actor validation, and system-level testing to ensure your product is stable and production-ready.

## Testing Your AALang Products with AATest

**AATest** is a comprehensive testing framework designed specifically for AALang products. It provides structured, message-based testing that evaluates test needs, generates test files, executes tests, and reports results.

### Overview

AATest follows the same **4-mode-13-actor** pattern as GAB, making it a natural companion for testing GAB-generated products. It supports three types of tests:

- **MessageResponseTest**: Tests how individual actors respond to messages - tests individual actor responsibilities in isolation
- **MessageFlowTest**: Tests message flow between actors, mode transitions, and state management - tests actor interactions
- **AgentWorkflowTest**: Tests complete agent workflows from user perspective - tests end-to-end workflows and full agent execution

All AATest tests are message-based: tests send AALang messages to actors and observe resulting messages, state changes, and behaviors. This aligns perfectly with AALang's message-passing architecture.

### Key Features

- **Automatic Test Generation**: Analyzes your AALang product and generates appropriate test files
- **LLM-Native Execution**: Tests execute within the LLM Agent's context, leveraging AALang's execution model
- **Comprehensive Assertions**: Supports semantic assertions, pattern matching, structural verification, and more
- **Test Fixtures and Mocks**: Built-in support for mock actors and test fixtures
- **Detailed Reporting**: Generates comprehensive test results with pass/fail status, execution logs, and summary statistics

### Using AATest

1. **Load AATest**: Add the `AATest/AATest.jsonld` and `AATest/AATest_spec.jsonld` files into your LLM environment
2. **Provide Product Path**: AATest will request the path to your AALang product file
3. **Follow the Workflow**: AATest guides you through Test Need Evaluation → Test Generation → Test Execution → Test Result Reporting
4. **Review Results**: Check the generated test results file for detailed execution logs

For complete documentation, see the [AATest README](AATest/README_AATest.md).

## Getting Started

### Prerequisites

- An LLM agent-based tool that can execute JSON-LD based prompts (e.g., Cursor, Claude Skills, home-made, etc.)
- Access to the GAB specification files

### Using GAB

1. **Load GAB**: Add the `*.jsonld` files into your LLM environment. The `*gab.jsonld*` file is the instruction file, the others are data files it needs. If you are in a tool like cursor, drag `*gab.jsonld*` into an empty chat and hit enter. If you are a standard LLM tool like gemini or ChatGPT, add all the files to a chat and indicate that `*gab.jsonld*` is the execution instructions. Gab works best in cursor-like tools.
2. **Describe Your Idea**: Tell GAB what you want to build
3. **Follow the Workflow**: GAB will guide you through Clarification → Discussion → Formalization → Generation
4. **Get Your Product**: Receive a complete AALang specification ready to use! **Note:** Your product is complete. None of the `* *.json*` or other files in this distribution are distributed with your product. 

#### User Commands

GAB supports several commands for managing your building process:

##### Decision Management
- `undo` - Undo the most recent decision
- `rollback to [N]` - Roll back to decision number N
- `show decisions` - View complete decision history

##### Actor Management
- `load actors` - Load all actors from generated .jsonld files
- `unload actors` - Return to builder-only mode
- `self-check actors` - Have loaded actors analyze their own instructions
- `skip formalization` - Explicitly authorize skipping Formalization Mode (Generation Mode is still mandatory)

### Example GAB Interaction

```
You: "I want to create a number guessing game where the LLM thinks of a number 
      and the user tries to guess it."

GAB: [Clarification Mode]
     "I understand you want a number guessing game. To clarify:
     - Should the number range be configurable?
     - How many guesses should the user have?
     - Should there be hints?"
     
You: [Answer questions]

GAB: [Discussion Mode]
     "Based on your answers, I propose a 2-mode architecture:
     - Setup Mode: Configure game parameters
     - Game Mode: Play the guessing game
     ..."
     
GAB: [Formalization Mode]
     "Analyzing the design for issues..."
     
GAB: [Generation Mode]
     "Generating your AALang product file..."
```

**AALang-based .jsonld files with standard startup instructions:**
- `gab.jsonld` - GAB (Generic AALang Builder) agent
- `AATest/AATest.jsonld` - AATest (AALang Testing Tool) agent



## Building Specific Product Types

### Creating Games

GAB excels at creating interactive games. Simply describe your game concept, and GAB will:
- Design the game modes and states
- Create actor personas for game logic
- Implement user interaction patterns
- Generate the complete game specification

**Example**: "Create a trivia game with multiple categories and difficulty levels"

### Creating Tools

Build tools that leverage LLM capabilities:
- Describe the tool's purpose
- Specify input/output requirements
- Define the workflow
- GAB generates the tool specification

**Example**: "Create a code review tool that analyzes code and provides suggestions"

### Creating Agents

Design custom LLM agents for specific tasks:
- Define the agent's purpose
- Specify modes and behaviors
- Design persona interactions
- Generate the agent specification

**Example**: "Create a customer support agent that handles common questions"

### Executing AALang Code

Once GAB generates your AALang specification:

1. **Load the Generated File**: Load the `.jsonld` file into your LLM
2. **Execute**: The LLM interprets and executes the AALang code
3. **Interact**: Use the product as designed

AALang specifications are **MCP and A2A ready**, meaning they integrate seamlessly with Model Context Protocol tooling for enhanced LLM interactions and support native Agent-to-Agent communication via gossip-based P2P protocols for distributed execution.



## Key Features

- **Structured Workflow**: Clear progression through Clarification → Discussion → Formalization → Generation
- **Quality Assurance**: Built-in checks for logic errors, edge cases, and AALang compliance
- **Decision Tracking**: All decisions are logged for review and rollback
- **Persona-Based Design**: Multiple personas provide diverse perspectives
- **MCP & A2A Ready**: Generated products work seamlessly with Model Context Protocol and support Agent-to-Agent communication for distributed execution
- **Flexible**: Build games, tools, agents, protocols, and more


## Using MCP (Model Context Protocol) with AALang and GAB

AALang is **MCP ready**, allowing you to integrate MCP servers into your AALang agents. GAB can automatically create the actors needed to interact with MCP servers.

### Setting Up an MCP Server Connection

1. **Configure MCP Server in Your LLM Tool**
   - Set up your MCP server connection in your LLM tool (e.g., Cursor, Claude Desktop, etc.)
   - Ensure the MCP server is running and accessible
   - Verify the connection is working by testing it in your LLM tool

2. **Identify Available MCP Tools**
   - Note which MCP tools/resources are available from your server
   - Understand what capabilities each tool provides
   - This information will help you describe your requirements to GAB

### Using MCP with GAB

During **Clarification Mode**, tell GAB in general terms how you want to use the MCP server connection:

**Example:**
```
You: "I want to create an agent that uses the filesystem MCP server to read and write files 
      based on user requests. The agent should be able to search for files, read their contents, 
      and create new files when requested."
```

GAB will:
- Understand your MCP integration requirements
- Ask clarifying questions about specific MCP tools you want to use
- Design the appropriate actors and modes needed for MCP interaction
- Create actors with the necessary capabilities to interact with your MCP server
- Generate AALang code that integrates seamlessly with your MCP setup

### What GAB Creates

GAB will automatically create:
- **Actors** with MCP tool calling capabilities
- **Modes** for different types of MCP interactions (if needed)
- **Message handling** for MCP tool requests and responses
- **Error handling** for MCP connection issues
- **State management** for MCP tool results

### Best Practices

- **Be specific about MCP tools**: Mention which specific MCP tools you want to use (e.g., "filesystem tools", "database tools", "API tools")
- **Describe use cases**: Explain what you want the agent to do with the MCP tools
- **Mention integration points**: If you need the agent to combine MCP tools with other capabilities, mention that during Clarification Mode
- **Test after generation**: Once GAB generates your agent, test the MCP integration to ensure it works as expected

**Note**: Your LLM tool must have MCP server connections configured. GAB creates the AALang agents that use those connections, but the MCP server setup itself is done in your LLM tool's configuration.

## File Structure

```
AALang-Gab-Development/
├── gab.jsonld              # Main GAB agent specification
├── gab-runtime.jsonld      # Runtime behaviors and instructions
├── gab-formats.jsonld      # Output and logging format definitions
├── aalang-design.jsonld    # Core AALang design specifications
├── agent-to-actor.jsonld   # Agent-to-actor communication protocols
├── message-protocol.jsonld # Message and state management protocols
├── README_support_docs/    # Documentation files referenced by README.md
│   ├── turing-complete.md      # Analysis of AALang's probabilistic Turing completeness
│   ├── concurrent-parallel.md # Analysis of AALang's concurrency and parallelism
│   └── ...                    # Other support documentation files
├── LICENSE                 # License information
└── README.md               # This file
```

## License

Copyright (c) 2025 Lee S. Barney

This software may be used, modified, and distributed for private, educational, and business purposes. The software files, modified versions, portions, and derivatives may NOT be sold or commercialized. Products built with AALang/GAB may be sold without restriction.

See [LICENSE](LICENSE) for full details.

---

**Ready to build?** Load `gab.jsonld` and start describing your idea!

