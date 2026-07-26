# PR-ready checklist (operator action: open PR manually)

Branch: `feature/zsh-rebased` on `/Users/agoodkind/Sites/shellcheck-zsh`
Base: `upstream/master` (koalaman/shellcheck)

## Gates (verified at coordinator follow-up)

- [x] Rebased onto upstream/master (7 zsh commits on current upstream)
- [x] `cabal test --allow-newer` PASS
- [x] `test/zsh/run-golden.sh` PASS (41 fixtures)
- [x] Zsh corpus: 2710/2914 chunks parse clean (~93%); baseline in `test/zsh/corpus-parse-failures.txt`
- [x] SC24xx audit committed (`3aa4a17`)
- [x] README, `shellcheck.1.md`, `--help` list zsh
- [x] CHANGELOG Git section documents zsh support
- [ ] Operator opens PR to koalaman/shellcheck (not opened by agents)

## Commits (linear history)

Run: `git log --oneline upstream/master..HEAD`

## Notes for reviewers

- SC2401 fires only on `T_GlobQualifier` (e.g. `*.txt(.)`), not bash extglob `*(.)`.
- Zsh corpus lives in gitignored `test/zsh/corpus/`; regenerate with `extract-ztst.sh` + `corpus-report.sh`.
- Local dev uses `cabal test --allow-newer` on GHC 9.14.x.
