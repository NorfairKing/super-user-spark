---
title: Specification
---
# Super user spark
## The general idea.
Configuring a system is hard work.
You don't want to do it all over again when you reinstall a system.
Moreover, you want to synchronise config files accross systems.


## Design ideas
### Spark cards
Spark is configured using spark cards.
These cards are written using a declarative language that describe the entire deployment.

### Configuration
Everything about Spark is configurable.
Every option can be controlled directly on the command-line *and* in the cards.

### Grammar
The grammar should be simple to use when you don't need many options, but clear when you do.


## Usage
### Options
#### Agressiveness
If spark encounters existing files at deploy destinations:

- Should it replace existing files? `--replace-files`
- Should it replace existing directories? `--replace-directories`
- Should it replace existing symbolic links? `--replace-links`
- Should it replace all of these? `--replace-all`

#### Copy or link.
By default, all unspecified deployments are links.

- A deployment with an unspecified deployment kind (`->`) can be configured to be a link `--link`, or a copy `--copy`.
- All deployment kind can be overridden as well: `--override-link` or `--override-copy`.


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
- By repository.
```
git <git-repository>:<branch-name> <file-path> <card-name>
```
The `:<branch-name>` `<file-path>` and `<card-name>` arguments are optional, but if `<card-name>` is given, `<filepath>` must be given as well.
If `<branch-name` is not given, the reference will lead to the master branch.
If `<file-path>` is not given, the reference will lead to the first `*.sus` file it can find in the root of the repository.
If `<card-name>` is not given the reference will lead to the first card in the file.


### Declarations
#### File deployment
This is the main operation.
```
source <deployment-kind-symbol> destination
```

##### Deployment Kind
There are two deployment kinds but three ways of declaring a deployment.

- Link: `l->`
- Copy: `c->`
- Unspecified: `->`

An unspecified deployment default to a link, but can be specified, both at the command line and in the card.

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
Successive `into` declarations will append to previous `into` declarations' directories.

#### Out of directory
After an `outof` declaration, all deployment destinations will be prefixed by the argument path.
```
outof <directory-path>
```
Successive `outof` declarations will append to previous `outof` declarations' directories.

#### Deployment Kind Override
What happens to unspecified deployments can be specified using a deployment kind override declaration:
```
kind <deployment-kind-symbol>
```

#### Alternatives
Alternative source directories can be specified.

```
alternatives <primary> <secondary> <tertiary> ... <last>
```

Spark will look for the source in the primary directory, then the secondary, etc.
This path is further prepended to the `into` directory.
