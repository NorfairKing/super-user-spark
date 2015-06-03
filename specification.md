# Grammar:
The grammar should be simple to use when you don't need many options, but clear when you do.



# Agressiveness
There should be a few settings that have to be configurable with regards to the agressiveness of spark.
- Should spark replace existing files?

It should be possible to configure these per card, per card recursive, per run or per deploy.

# Deployable: file 
A file can be symlinked or copied,
It should be possible to specify either in the grammer.
This specification should be overridable by a command-line flag.

`spark --copy` overrides all file deploying to be copied.
`spark --link` overrides all file deploying to be linked.

it should be possible to configure which permissions the file should get.


An obvious operator sign would be an arrow: '->'
```
the original file in the depot -> the destination
```

# Card:
A card can be a file, that is, a file can contain ONE card, or it can contain many cards.

When only one card is contained in a file, the filename will be the name of the card.
When there is more than one card in a file, the name should be specified.
Do cards even need names? (yes probably, but we'll make them optional)

Here's an idea for a syntax:

``` Full-fledged
card "name of this card" {
}
```

``` without a name, the name will be "filename-<number of the card in the file>"
card {
}
```

this last idea would come in very handy to go with unnamed cards in files without a card declaration.

# Sparkoffs
It should be possible to spark off another card
- From the same file
- From another file
 - relative to this card
 - with an absolute path
- From a git directory that has not been fetched yet.

# Sparking 
To execute the deployer, the command should look like this:

`spark <card file path>`
`spark git <repository> <optional card name>`  (otherwise something specified in the repo)
`spark <card name>` (by name)


It should be possible to add a card to the card directory so that it can be sparked by name.

