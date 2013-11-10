{-
    This file is part of ShellCheck.
    http://www.vidarholen.net/contents/shellcheck

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
import ShellCheck.Simple
import Text.JSON

instance JSON ShellCheckComment where
  showJSON c = makeObj [
      ("line", showJSON $ scLine c), 
      ("column", showJSON $ scColumn c),
      ("level", showJSON $ scSeverity c),
      ("code", showJSON $ scCode c),
      ("message", showJSON $ scMessage c)
      ]
  readJSON = undefined

main = do
  script <- getContents
  putStrLn $ encodeStrict $ shellCheck script 
