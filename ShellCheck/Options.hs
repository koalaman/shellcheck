module ShellCheck.Options where

data Shell = Ksh | Sh | Bash
    deriving (Show, Eq)

data AnalysisOptions = AnalysisOptions {
    optionShellType :: Maybe Shell,
    optionExcludes :: [Integer]
}

defaultAnalysisOptions = AnalysisOptions {
    optionShellType = Nothing,
    optionExcludes = []
}
