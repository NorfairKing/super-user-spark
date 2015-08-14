---
title: The general idea
---

Configuring a system is hard work.
You don't want to do it all over again when you reinstall a system.
Moreover, you want to synchronise config files accross systems.


## Design ideas
### Usage
The most used options should require the least amount of characters to type.

### Spark cards
Spark is configured using spark cards.
These cards are written using a declarative language that describe the entire deployment.

### Configuration
Everything about Spark is configurable.
Every option can be controlled directly on the command-line *and* in the cards.
You should be able to set up your cards once and deploy them many times.

### Grammar
The grammar should be simple to use when you don't need many options, but clear when you do.

