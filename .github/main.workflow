workflow "on push" {
  on = "push"
  resolves = ["Haskell Linter"]
}

action "Haskell Linter" {
  uses = "domdere/haskell-lint-action@master"
}
