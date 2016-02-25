---
title: Frequently Asked Questions
---

- When I enter `spark parse some-file`, I get the following error, what does it mean?
    ```
    "some-file" (line ..., column ...):
    unexpexted end of input
    expecting Comments
    ```
    
    The first thing `spark` does when parsing a file, is removing all the comments.
    If your file is so badly formatted that even this goes wrong, you're probably trying to parse the wrong file.

- When I try to use the variable `HOST`, spark cannot resolve it eventhough it is there when I enter `echo $HOST`.
  
  This means the variable `HOST` has not been exported.
  Bash does not do this automatically.
  You can export `HOST` manually with `export HOST`.
