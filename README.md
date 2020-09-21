# oregex

A simple OCaml regexp library. Implemented using lazily evalutated NFAs.

## Features

Currently the library supports the following regex expression types
Feature | Symbol | Description | Example
--- | --- | --- | ---
Character matching | `a` | Matches against a given character | `a` matches `'a'`
Wildcard | `.` | Matches against any character | `.` matches `'a'` or `'b'`
Alternation | `|` | Matches against either the item on the left or right | `a|b` matches `'a'` or `'b'`
Zero or more repitition | `*` | Matches against zero or more occurences of the preceeding token | `a*` matches `''` or `'a'` or `'aa'`...
One or more repitition | `+` | Matches against one or more occurences of the preceeding token | `a+` matches `'a'` or `'aa'`...
