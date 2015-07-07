---
title: Usage
---

## Commands
### Deployment
Deployment doesn't require any special commands, just a card reference.

You can reference a starting card either [by file](#file-reference) or [by git repository](#git-reference):
```
$ spark file path/to/card.sus card-name # By file
$ spark git git@github.com:NorfairKing/sus-depot.git spark.sus "sus depot" # By git reference
```

#### Deployment options
TODO

### Syntax checking
A card's syntax can be checked by entering the following commands:

```
$ spark check-syntax /path/to/card.sus
```

`spark` will exit...

- ... with exit code 0 to show that the file is syntactically correct
- ... with a non 0 exit code to show that the file contains a syntax error and give some indication as to where the error is.


### Formatting
Formatting is used to format a card into a pretty representation.
This mostly includes nice whitespace.

```
$ spark format /path/to/card
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
- `--always-quote`: adds quotation marks around every path.

```
"abc"      -> "ghi jkl"
"abc def"  -> "def"
```

Instead of this (default):

```
 abc       -> "ghi jkl"
"abc def"  ->  def
```

### Compiling 
Compile a spark card.
This, unlike deployment, happens independtly of the system that `spark` is being run on.

```
$ spark compile path/to/card.sus
$ spark compile path/to/card.sus card-name
```

This will compile the spark card to a set of deployments.

#### Compiling options

- `-o FILE` or `--output FILE` output to a `FILE` instead of stdout.
- `--format FORMAT`: Compile to a specific format. (Options for `FORMAT`: `binary`(default), `text` or `json`)
- `--standalone`: Compile to a standalone binary to be run seperately instead of a set of deployments.


### Check

Assess the current state of the system.
Look at what is done already and what needs to be done.


#### Check options

- `--depth OPTION`: How far to go in checking whether files and directories are already deployed. (Options for `OPTION`: `name-only`, `checksum` `content`(default))


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

