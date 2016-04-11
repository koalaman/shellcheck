{-
    Copyright 2012-2015 Vidar Holen

    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module ShellCheck.Analyzer (analyzeScript) where

import ShellCheck.Analytics
import ShellCheck.AnalyzerLib
import ShellCheck.Interface
import Data.List
import qualified ShellCheck.Checks.Commands


-- TODO: Clean up the cruft this is layered on
analyzeScript :: AnalysisSpec -> AnalysisResult
analyzeScript spec = AnalysisResult {
    arComments =
        filterByAnnotation (asScript spec) . nub $
            runAnalytics spec
            ++ ShellCheck.Checks.Commands.runChecks spec
}
