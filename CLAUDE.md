# Repository rules

## main is off-limits

Do not merge into `main`, push commits to `main`, or make any change that
alters code on `main` — regardless of what branch this session started on
or what task is in progress. This applies even if a user message earlier
in a session approved a merge or push to `main`; that approval does not
carry forward. If a task seems to require touching `main` (a merge, a
direct push, a rebase onto it, deleting/creating files there), stop and
ask before doing anything, rather than proceeding.

Work happens on feature/chore branches. Push those freely. `main` is
read-only from here.
