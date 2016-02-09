---
title: Technical Details
---

```
             |    parse    |   compile               | predeploy -> deploy -> check
-------------+-------------+------------------------------------------
     IO part |  read file  |  find files
non- IO part |  parse AST  | compile to deployments 
```

# Stages
## Parse
    IO part: read file
non-IO part: parse string into AST

## Compile
    IO part: find files recursively
non-IO part: Compile into deployments

The only product of compilation is a list of deployments.
Because the compilation is entirely modular, the IO part can be separated entirely.
Compilation of a given compilation unit given a compilation scope can be entirely pure.
It will either return a finished list of deployments or a Filepath leading to the next unit to compile.

## Predeploy

## PostDeploy

## Check
