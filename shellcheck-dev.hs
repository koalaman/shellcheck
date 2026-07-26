{-
    Copyright 2026 Vidar Holen

    This file is part of ShellCheck.
    https://www.shellcheck.net

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

-- shellcheck-dev is primarily meant for the potential benefits of AI.
-- It can be run with `cabal run -fdev-mode shellcheck-dev -- ast 'myshellcommand'`

import ShellCheck.Debug
import System.Environment
import System.Exit
import System.IO
import Data.List
import qualified Data.Map as Map

commands :: Map.Map String (String -> String)
commands = Map.fromList [
    ("ast", show . stringToAst)
    ]

validCommands :: String
validCommands = intercalate ", " $ Map.keys commands

putStrLnErr = hPutStrLn stderr

main = do
    args <- getArgs
    case args of
        [cmd, arg] ->
            case Map.lookup cmd commands of
                Just f -> putStrLn $ f arg
                Nothing -> do
                    putStrLnErr $ "Unknown command. Try one of: " ++ validCommands
                    exitFailure
        _ -> do
            putStrLnErr $ "Usage: shellcheck-dev command argument, where command is: " ++ validCommands
            exitFailure
