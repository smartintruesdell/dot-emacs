# Bespoke ideas

## `my/gptel-commit`
Read `git diff --cached`, send to gptel, paste result into the magit commit buffer.
Both pieces exist (gptel + magit) — just needs the wire between them.

## `my/vterm-project`
`C-c t` opens a named `*vterm:<project>*` buffer rooted at the project root.
Calling it again from anywhere in the project toggles back to the same buffer.

## `my/consult-errors`
Single consult/vertico picker that aggregates eglot type errors + flymake-eslint
lint violations with live-preview jumping. Right now those two sources require
separate navigation.

## `my/ts-organize-imports-on-save`
Before-save hook that fires `source.organizeImports` eglot code action in
tsx-ts-mode / typescript-ts-mode buffers only. Eliminates unused-import noise
without manual intervention.

## `my/copy-github-link`
Build `https://github.com/<owner>/<repo>/blob/<sha>/<file>#L<line>` and
put it in the kill ring. forge knows the remote URL, `git rev-parse HEAD`
gives the SHA.
