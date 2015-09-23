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

### Formatting
Formatting is used to format a `.sus` file into a pretty representation.
This mostly includes nice whitespace.

```
$ spark format path/to/card.sus
```

#### Formatting options
- `--no-line-up`: don't line up deployments.

```
abc -> cde
abcdefg c-> xyz
defg l-> "hijk lm"
"abc def" c-> jik
```

Instead of this (default):

```
 abc       ->  cde
 abcdefg  c->  xyz
 defg     l-> "hijk lm"
"abc def" c->  jik
```

- `--indent n`: Indent each new block with `n` extra spaces. (default `n = 4`)
- `--no-trailing-newline`: Don't add a trailing newline to the file. `spark format` does so by default.
- `--always-quote`: adds quotation marks around every path and every name.

```
"abc"      -> "ghi jkl"
"abc def"  -> "def"
```

Instead of this (default):

```
 abc       -> "ghi jkl"
"abc def"  ->  def
```

- `--compress`: Compress the card as much as possible. This will put the entire card on one line with the least amount of characters possible.

This option will convert this card:

```
card card-name {
    into ~
    outof directory
    kind copy
    alternatives one two

    "file one"   -> file-two
     something  c-> copied
}
```

into this:

```
cardcard-name{into~;outofdirectory;kindcopy;alternativesone two;"file-one"-> file-two;something c-> copied}
```

### Compiling 
Compile a spark card.
This, unlike deployment, happens independtly of the system that `spark` is being run on.

```
$ spark compile path/to/card.sus
$ spark compile "path/to/card.sus card-name"
```

Not that the quotes are required if you specify a card name.
This will compile the spark card to a list of deployments.

#### Compiling options

- `--output FILE` output to a `FILE` instead of stdout.
- `--format FORMAT`: Compile to a specific format. (Options for `FORMAT`: `binary`, `text`(Default), `json` or `standalone`*(NYI)*) When set to `standalone`, compile to a standalone binary to be run seperately instead of a set of deployments.
- `--kind KIND`: Specify any unspecified deployments (`->`) to be `KIND` deployments (`c->` or `l->`). (Options for `KIND`: `copy`, `link`)
- `--override KIND`: Override _all_ deployments to be `KIND` deployments. (Options for `KIND`: `copy`, `link`)


### Check

Assess the current state of the system.
Look at what is done already and what needs to be done for a given card to be deployed

You can reference a starting card either by file or by git repository:

```
$ spark check path/to/card.sus
$ spark check "path/to/card.sus card-name"
```

Not that the quotes are required if you specify a card name.

You can also supply a compiled card:

```
$ spark check path/to/compiled/card
```

Remember, when using compiled data, if you used a specific `--format`, you have to use that option again.
Don't use the `.sus` extension for compiled cards or `spark` will interpret them as uncompiled card files.

#### Check options

- `--thoroughness OPTION`: How far to go in checking whether files and directories are already deployed. (Options for `OPTION`: `name`, `checksum` `content`(default)) *(NYI)*

### Deployment
Deployment doesn't require any special commands, just a card reference.

You can reference a starting card either by file or by git repository:

```
$ spark deploy path/to/card.sus # By file
$ spark deploy "path/to/card.sus card-name" # By file with card name
```

Not that the quotes are required if you specify a card name.

You can also supply a compiled card:

```
$ spark deploy path/to/compiled/card
```

Remember, when using compiled data, if you used a specific `--format`, you have to use that option again.
Don't use the `.sus` extension for compiled cards or `spark` will interpret them as uncompiled card files.

#### Deployment options

- `--replace-files`: replace existing files at deploy destinations
- `--replace-directories`: replace existing directories at deploy destinations
- `--replace-links`: replace links at deploy destinations
- `--replace`: equivalent to `--replace-files --replace-directories --replace-links`
