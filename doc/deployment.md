---
title: Deployment
---

What are the possible situations:

## The deployment is a file
### Before
#### Good
- The source is a file
- The source has read permissions
- The destination doesn't exist.
- The destination parent directory has execute permissions.
- The destination parent directory has write permissions.

#### Bad
- The source has no read permissions.
- The source is a directory.
- The source does not exist.
- The destination already exists.
- The destination already exists and is a directory.
- The destination already exists and is a symbolic link.
- The destination parent directory has no execute permissions.
- The destination parent directory has no write permissions.

### During
#### Bad
- Any error

### After
#### Good
- The source and destination are both files.
- The source and destination have equal contents.

#### Bad
- The destination is not a file.

## The deployment is a directory
### Before
#### Good
- The source is a directory.
- The source has execute permissions
- The destination doesn't exist.
- The destination parent directory has execute permissions.
- The destination parent directory has write permissions.

#### Bad
- The source is a file
- The source has no execute permissions.
- The source does not exist.
- The parent directory of the destination doesn't exist.
Automatic fix: make the parent directories.
(configurable: don't make parent directories automatically.)
- The destination already exists.
- The destination already exists and is a file.
- The destination already exists and is a symbolic link.
- The destination parent directory has no execute permissions.
- The destination parent directory has no write permissions.

### During
#### Bad
- Any error

### After
#### Good
- The source and destination are both directories.
- The source and destination have equal contents.

#### Bad
- The destination is not a directory

