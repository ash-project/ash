# AALang Actor Execution Mechanics

## Understanding How LLMs Execute AALang Actors

This document explains how Large Language Models (LLMs) execute AALang actor definitions in practice, helping you understand the practical mechanics of AALang agent behavior.

## Core Concept: Definition Adoption, Not Instance Creation

When an LLM executes an AALang agent specification, it doesn't create separate actor instances or blank templates. Instead, the LLM **adopts** actor definitions dynamically as needed.

### What This Means

1. **Actor definitions are read and understood**: The LLM parses the JSON-LD file containing actor definitions (responsibilities, personas, modes, constraints, etc.)

2. **Definitions are stored in context**: The actor specifications become part of the LLM's working context

3. **Behavior is adopted dynamically**: When the LLM needs to act as a specific actor, it adopts that actor's definition—changing its behavior, reasoning, and responses to match that actor's responsibilities and persona

4. **Single LLM, multiple behaviors**: The same LLM instance can adopt different actor definitions at different times, effectively "becoming" different actors as needed

## Practical Example: Loading Actors

When you execute `load actors`:

```
1. LLM reads: number-guessing-game.jsonld
2. LLM parses: Actor definitions (GameMaster, Player, etc.)
3. LLM stores: Actor responsibilities, personas, modes in context
4. LLM adopts: When GameMaster should respond, LLM adopts GameMaster definition
5. LLM adopts: When Player should respond, LLM adopts Player definition
```

The LLM doesn't create separate GameMaster and Player instances. Instead, it dynamically adopts each definition when that actor needs to act.

## Why This Matters for AALang Design

Understanding this execution model helps you design better AALang agents:

### 1. **Clear, Self-Contained Definitions**
Each actor definition must be complete and unambiguous because the LLM will adopt it directly. Vague or incomplete definitions lead to inconsistent behavior.

### 2. **Explicit Responsibilities**
Responsibilities should be explicit and actionable. The LLM uses these as direct instructions for behavior.

### 3. **Persona Consistency**
Persona definitions (personality, role, mode) guide the LLM's tone, reasoning style, and decision-making when adopting that actor.

### 4. **Mode-Based Behavior**
Mode definitions help the LLM understand when to adopt which actor behaviors and what constraints apply.

## Implications for Agent Designers

### ✅ Do:
- Write clear, explicit actor responsibilities
- Define personas with distinct characteristics
- Use modes to organize actor behavior
- Make constraints and prohibitions explicit
- Design for context-window native execution (no polling, no separate processes)

### ❌ Don't:
- Assume actors run as separate processes
- Design for explicit inter-process communication
- Rely on shared mutable state between actors
- Use polling or monitoring patterns
- Create actors that require separate execution contexts

## How Self-Check Works

When executing `self-check actors`:

1. **LLM adopts Actor A's definition**: Analyzes Actor A's instructions from Actor A's perspective
2. **LLM reports as Actor A**: Reports findings as if Actor A discovered them
3. **LLM adopts Actor B's definition**: Switches context, analyzes Actor B's instructions
4. **LLM reports as Actor B**: Reports findings as if Actor B discovered them

Each actor's self-check is performed by the LLM adopting that actor's definition and analyzing from that perspective.

## Context-Window Native Design

AALang is designed for **context-window native execution**:

- Messages exist in the LLM's context window
- Actors "see" messages through semantic filtering (natural language understanding)
- No explicit message queues or polling needed
- State is managed through message-based protocols
- Everything happens within the LLM's context window

This is why AALang avoids:
- Explicit polling/monitoring instructions
- Separate process communication
- Shared mutable state
- Request-response acknowledgment patterns

Instead, AALang uses:
- Semantic message filtering
- Isolated state per actor/mode
- Message-based state updates
- Context-window native processing

## Benefits of This Model

1. **Simplicity**: No complex inter-process communication
2. **Flexibility**: Easy to modify actor definitions
3. **Transparency**: All behavior defined in JSON-LD
4. **Portability**: Works with any LLM that can parse JSON-LD
5. **Debuggability**: All actor behavior is explicit in definitions

## For Tool Builders

If you're building tools that work with AALang agents:

- **Read JSON-LD directly**: Parse actor definitions from JSON-LD files
- **Understand adoption model**: Tools should understand that actors are definitions, not instances
- **Support context-window processing**: Tools should work with context-window native patterns
- **Respect isolated state**: Tools should understand mode-based state isolation
- **Follow message protocols**: Tools should implement AALang message protocols

## Conclusion

AALang actors are **behavioral definitions** that LLMs adopt dynamically, not separate running processes. This model enables:

- Simple, declarative agent specifications
- Context-window native execution
- Easy modification and iteration
- Clear, explicit behavior definitions

Understanding this execution model helps you design better AALang agents and build better tools that work with AALang.

---

*Created using AALang and Gab*

