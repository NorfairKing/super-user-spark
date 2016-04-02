---
title: Usage
abbreviations: NYI="Not Yet Implemented"
---

## Commands

### Syntax checking
A `.sus` file's syntax can be checked by entering the following command:

```
$ spark parse path/to/card.sus
```

`spark` will exit...

- ... with exit code 0 to show that the file is syntactically correct
- ... with a non 0 exit code to show that the file contains a syntax error and give some indication as to where the error is.

`spark` will try its best to present you with a good indication of what went wrong but as with all parsers, errors can still seem cryptic.

Note that the parser will not follow `spark` declarations in the file. It's just a parser.

### Compiling 
Compile a spark card.
This, unlike deployment, happens independtly of the system that `spark` is being run on.

```
$ spark compile path/to/card.sus
$ spark compile "path/to/card.sus card-name"
```

Note that the quotes are required if you specify a card name.
This will compile the spark card to a list of deployments.

#### Compiling options

- `--output FILE` output to a `FILE` instead of stdout.
- `--kind KIND`: Specify any unspecified deployments (`->`) to be `KIND` deployments (`c->` or `l->`). (Options for `KIND`: `copy`, `link`)
- `--override KIND`: Override _all_ deployments to be `KIND` deployments. (Options for `KIND`: `copy`, `link`)


### Check

Assess the current state of the system.
Look at what is done already and what needs to be done for a given card to be deployed

You can reference a starting card by file:

```
$ spark check path/to/card.sus
$ spark check "path/to/card.sus card-name"
```

Note that the quotes are required if you specify a card name.

You can also supply a compiled card:

```
$ spark check path/to/compiled/card
```

Don't use the `.sus` extension for compiled cards or `spark` will interpret them as uncompiled card files.

### Deployment
Deployment doesn't require any special commands, just a card reference.

You can reference a starting card by file:

```
$ spark deploy path/to/card.sus # By file
$ spark deploy "path/to/card.sus card-name" # By file with card name
```

Note that the quotes are required if you specify a card name.

You can also supply a compiled card:

```
$ spark deploy path/to/compiled/card
```

Don't use the `.sus` extension for compiled cards or `spark` will interpret them as uncompiled card files.

#### Deployment options

- `--replace-files`: replace existing files at deploy destinations
- `--replace-directories`: replace existing directories at deploy destinations
- `--replace-links`: replace links at deploy destinations
- `--replace-all`: equivalent to `--replace-files --replace-directories --replace-links`
