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
- The source is a symbolic link.
- The source has no read permissions.

### Destination
#### Good
- The destination doesn't exist.
- The destination parent directory has write permissions.

#### Bad
- The destination already exists and is a file.
- The destination already exists and is a directory.
- The destination already exists and is a symbolic link.
- The parent directory of the destination doesn't exist.
- The parent directory of the destination has no write permissions.

## During deployment
#### Bad
- There is not enough space on the destination device to deploy the file or directory.

## After a depoyment
### Destination
#### Good
- The destination is a file iff the source is a file.
- The destination is a directory iff the source is a directory.
- The source and destination's contents are equal.
