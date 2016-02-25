---
title: Language Specification
---

## The language
### Cards
A card is a unit of control within spark.
A card has a name and contains a number of statements.
Cards should be declared inside a file with a `.sus` extension.

#### References
A card can be referenced:

- By name.

```
card <card-name>
```

- By file.

```
file <file-path> <card-name>
```

The `<card-name>` argument is optional, if it is not given the reference will lead to the first card in the file.

### Declarations
#### File deployment
This is the main operation.

```
<source> <deployment-kind-symbol> <destination>
```

This will deploy `<source>` to `<destination>`.

There is also a shorthand syntax:

```
<file>
```

This deploys `<file>` to `<file>`.
It works very well if the name of the source and destination files are the same.

Because spark is mainly used to deploy *dot*files, there is an implicit shorthand if the destination starts with a dot:

```
.<file>
```

Along with the regular `.<file>` to `.<file>` deployment, this will result in an implicit alternative `<file>` to `.<file>` deployment if the first one doesn't work out.

##### Deployment Kind
There are two deployment kinds but three ways of declaring a deployment.

- Link: `l->`
- Copy: `c->`
- Unspecified: `->`

An unspecified deployment defaults to a link, but can be specified otherwise both at the command line and in the card.

#### Sparkoff
A sparkoff is used to run spark on other cards from within a card.

```
spark <card identifier>
```

If the referenced card is in a remote repository, spark will fetch the repository within the directory that spark is run in.

#### Into directory
After an `into` declaration, all deployment sources will be prefixed by the argument path.

```
into <directory-path>
```

Successive `into` declarations will append to previous `into` declarations' directories (intersperced with `/`).

#### Out of directory
After an `outof` declaration, all deployment destinations will be prefixed by the argument path.

```
outof <directory-path>
```

Successive `outof` declarations will append to previous `outof` declarations' directories (intersperced with `/`).

#### Deployment Kind Override
What happens to unspecified deployments can be specified using a deployment kind override declaration:

```
kind copy
```

or

```
kind link
```

#### Alternatives
Alternative source directories can be specified.

```
alternatives <primary> <secondary> <tertiary> ... <last>
```

Spark will look for the source in the primary directory, then the secondary, etc.
This path is further prepended to the `into` directory.

### Block

A block is a part of the code between braces `{`, `}`.
It is used to scope declaratons:

```
{
  into a
  b
}
c
```

... will result in ...

```
b -> a/b
c -> c
```


### Comments
Line comments start with `#` and block comments are surrounded by `[[` and `]]`
