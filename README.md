# Pinned Warnings

### The problem
When working in GHCi, it's easy to lose track of warnings from modules other
than the one you are currently editing. Having to reload the project to catch
any warnings you might have missed is painful.

### The solution
This package has a GHC plugin that allows you to see all the current warnings
from all modules in GHCi by calling a special `showWarnings` function.

### Usage
You can start GHCi in your project with one of the following commands to enable
the necessary plugin. Add `pinned-warnings` as a package dependency to avoid
having to include the additional argument.
```
cabal update
cabal new-repl -b pinned-warnings --repl-options="-fplugin PinnedWarnings"

stack update
stack repl --package pinned-warnings --ghci-options "-fplugin PinnedWarnings"
```
Then you must add the `ShowWarnings` module to the GHCi context:
```
:m + ShowWarnings
```
Now all active warnings can be viewed by evaluating `showWarnings`.

### Tip
You can define a custom GHCi command in your `.ghci` file that adds the
`ShowWarnings` module and calls `showWarnings`:
```
:def sw (\_ -> pure ":m + ShowWarnings \n showWarnings")
```
With this you can simply use `:sw` to view warnings.

### Fixing warnings
There is limited functionality for automatically fixing warnings. Currently
only warnings due to redundant module import statements are corrected but there
is room for improvement in this regard.

To use this functionality, have the `ShowWarnings` module added to your GHCi
session then evaluate `fixWarnings`.

### Known limitations
- Warnings that aren't for a specific module are not captured.
- Only the versions of GHC specified in the cabal file are supported (8.10, 9.0, 9.2)
- If you make changes to files and call `showWarnings` without reloading first,
  the messages may not be referencing the right code anymore.
