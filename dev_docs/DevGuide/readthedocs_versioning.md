# How Read the Docs Is Updated

Read the Docs (<https://musclex.readthedocs.io/>) only rebuilds the
published documentation in **two cases**:

1. **A commit is pushed to the default branch (`master`).**
   This rebuilds the `latest` version and replaces the previous
   `latest` content.

2. **A "stable" tag is pushed.**
   This rebuilds the `stable` version. Whether a tag counts as stable
   is decided by Read the Docs' own rule (semver-compatible tag, no
   pre-release suffix, highest one wins). See:
   <https://docs.readthedocs.com/platform/stable/versions.html>.

> Note: The only "automatic" deactivation/deletion event is when the
> underlying Git branch or tag is deleted from the repo (and even then
> it requires an automation rule to act on active versions).
