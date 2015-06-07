---
title: Deployment
---

What are the possible situations:

## Before a deployment
### Source
#### Good
- The source is a file.
- The source is a directory.
- The source has read permissions.

#### Bad
- The source does not exist.

Panic

- The source is a symbolic link.

Warning

- The source has no read permissions.

Panic
(configurable: fix this automatically if current user is owner)

### Destination
#### Good
- The destination doesn't exist.
- The destination parent directory has write permissions.

#### Bad
- The destination already exists and is a file.

Panic
(configurable: replace existing file.)

- The destination already exists and is a directory.

Panic
(configurable: replace existing directory.)

- The destination already exists and is a symbolic link.

Panic
(configurable: unlink the existing symbolic link.)

- The parent directory of the destination doesn't exist.

Automatic fix: make the parent directories.
(configurable: don't make parent directories automatically.)

- The parent directory of the destination has no write permissions.

Panic
(configurable: fix this automatically if current user is owner)

## During deployment
#### Bad
- There is not enough space on the destination device to deploy the file or directory.

Panic

## After a depoyment
### Destination
#### Good
- The destination is a file iff the source is a file.
- The destination is a directory iff the source is a directory.
- The source and destination's contents are equal.
