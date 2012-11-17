#!/usr/bin/env runhaskell
-- #!/usr/bin/env runhugs
-- 	$Id: quickcheck,v 1.4 2003/01/08 15:09:22 shae Exp $	
-- This file defines a command
--      quickCheck <options> <files>
-- which invokes quickCheck on all properties defined in the files given as
-- arguments, by generating an input script for hugs and then invoking it.
-- quickCheck recognises the options
--      +names     print the name of each property before checking it
--      -names     do not print property names (the default)
--      +verbose   displays each test case before running
--      -verbose   do not displays each test case before running (the default)
-- Other options (beginning with + or -) are passed unchanged to hugs.
--
-- Change the first line of this file to the location of runhaskell or runhugs
-- on your system.
-- Make the file executable.
--
-- TODO:
-- someone on #haskell asked about supporting QC tests inside LaTeX, ex. \{begin} \{end}, how?

import System.Cmd
import System.Directory (findExecutable)
import System.Environment
import Data.List
import Data.Maybe (fromJust)

main :: IO ()
main = do as<-getArgs
          sequence_ (map (process (filter isOption as)) 
	                 (filter (not.isOption) as))

-- ugly hack for .lhs files, is there a better way?
unlit [] = []
unlit x  = if (head x) == '>' then (tail x) else x

process opts file =
       let (namesOpt,opts') = getOption "names" "-names" opts
           (verboseOpt,opts'') = getOption "verbose" "-verbose" opts' in
       do xs<-readFile file
          let names = nub$ filter (\x -> (("> prop_" `isPrefixOf` x) || ("prop_" `isPrefixOf` x)))
	                (map (fst.head.lex.unlit) (lines xs)) 
          if null names then
	      putStr (file++": no properties to check\n")
	    else do writeFile "hugsin"$
	              unlines ((":load "++file):
                          ":m +Test.QuickCheck":
                          "let quackCheck p = quickCheckWith (stdArgs { maxSuccess = 1 }) p ":
	                       [(if namesOpt=="+names" then 
			           "putStr \""++p++": \" >> "
				 else "") ++
				("quackCheck ")
				++ p | p<-names])
                    -- To use ghci
                    ghci <- findExecutable "ghci"
	            system (fromJust ghci ++options opts''++" <hugsin")
	            return ()

isOption xs = head xs `elem` "-+"

options opts = unwords ["\""++opt++"\"" | opt<-opts]

getOption name def opts = 
  let opt = head [opt | opt<-opts++[def], isPrefixOf name (drop 1 opt)] in
    (opt, filter (/=opt) opts)
