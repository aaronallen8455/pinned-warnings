# Pinned Warnings

### The problem
When working in GHCi, it's easy to lose track of warnings if they are from
modules other than the one you are currently editing.

### The solution
This package has a GHC plugin that allows you to see all the current warnings
from all modules in GHCi by calling the magic `showWarnings` function.
