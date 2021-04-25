# Pinned Warnings

### The problem
When working in GHCi, it's easy to lose track of warnings from modules other
than the one you are currently editing. If you have ever restarted GHCi just to
check for warnings may have gotten lost in the shuffle, this is for you.

### The solution
This package has a GHC plugin that allows you to see all the current warnings
from all modules in GHCi by calling the magic `showWarnings` function.

### Usage
You can start GHCi in your project with one of the following commands to enable
the necessary plugin. You can add `pinned-warnings` as a package dependency to
avoid having to include the additional argument.
```
cabal update
cabal new-repl -b pinned-warnings --ghc-options="-fplugin PinnedWarnings"

stack update
stack repl --package pinned-warnings --ghci-options "-fplugin PinnedWarnings"
```
You then need to add the `ShowWarnings` module to the GHCi context:
```
:m + ShowWarnings
```
Now all active warnings can be viewed by calling `showWarnings`.

### Tip
You can define a custom GHCi command in your `.ghci` file that adds the
`ShowWarnings` module and calls `showWarnings`:
```
:def sw (\_ -> pure ":m + ShowWarnings \n showWarnings")
```
Now you can simply use `:sw` to view warnings.

### Known limitations
- Warnings produced outside of module type checking are not captured,
  for example those related to `.cabal` file omissions.
- Only the specified versions of GHC are supported
