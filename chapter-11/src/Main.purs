module Main where

import Prelude
import Control.Monad.Eff         (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Reader      (Reader, ask, local, runReader)
import Control.Monad.State       (State, execState, runState, evalState)
import Control.Monad.State.Class (modify)
import Data.Array                (replicate)
import Data.Foldable             (traverse_)
import Data.String               (toCharArray, joinWith)
import Data.Traversable          (sequence)
import Data.Tuple                (Tuple(..))


sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

sumArrays :: State Number Unit
sumArrays = do
    sumArray [ 1.0, 2.0, 3.0 ]
    sumArray [ 4.0, 5.0      ]
    sumArray [ 6.0           ]


withExec :: Number
withExec =  execState sumArrays 0.0

-- Chapter 11, `State` monad exercise 1:
-- (Easy) What is the result of replacing execState with runState or evalState
-- in our example above?

withRun :: Tuple Unit Number
withRun =  runState sumArrays 0.0

withEval :: Unit
withEval =  evalState sumArrays 0.0


-- Chapter 11, `State` monad exercise 2:
-- (Medium) A string of parentheses is balanced if it is obtained by either
-- concatenating zero-or-more shorter balanced strings, or by wrapping a
-- shorter balanced string in a pair of parentheses.
--
-- Use the `State` monad and the `traverse_` function to write a function
--
--  `testParens :: String -> Boolean`
--
-- which tests whether or not a String of parentheses is balanced, by keeping
-- track of the number of opening parentheses which have not been closed. Your
-- function should work as follows:
--
-- ```
--  > testParens ""
--  true
--
--  > testParens "(()(())())"
--  true
--
--  > testParens ")"
--  false
--
--  > testParens "(()()"
--  false
-- ```
--
-- Hint: you may like to use the `toCharArray` function from the `Data.String`
-- module to turn the input string into an array of characters.

testParens :: String -> Boolean
testParens ps = execState st 0 == 0
    where track u '(' | u < 0     = u
                      | otherwise = u + 1
          track u ')' = u - 1
          track u _   = u

          st = traverse_ (\c -> modify \u -> track u c) (toCharArray ps)


testParensLog :: forall e. Eff (console :: CONSOLE |e) Unit
testParensLog = do
    log $  "11.4.2 - Use the `State` monad and the `traverse_`"
        <> " function to check for balanced parentheses:"
    foreachE ls log'

    where ls = [ Tuple ""            true
               , Tuple "(()(())())"  true
               , Tuple ")"           false
               , Tuple "(()()"       false
               , Tuple ")("          false
               , Tuple "(()))(()())" false
               ]

          expect a b
              | a == b    = "Passed"
              | otherwise = "Failed!"

          log' (Tuple ps e) =
              log $  "  `testParens \""
                  <> ps
                  <> "\"` should be `"
                  <> show e
                  <> "`\n    "
                  <> (expect e $ testParens ps)
                  <> "\n"


--------------------------------------------------------------------------------

-- Chapter 11, `Reader` monad exercise 1:
-- (Easy) Write a function `line` which renders a function at the current
-- indentation level. Your function should have the following type:

--  `line :: String -> Doc`

-- Hint: use the `ask` function to read the current indentation level.

type Level = Int
type Doc   = Reader Level String

line :: String -> Doc
line l = do
    lvl <- ask
    let indents = joinWith "" $ replicate lvl "  "
    pure $ indents <> l


-- Chapter 11, `Reader` monad exercise 2:
-- (Easy) Use the `local` function to write a function

--  `indent :: Doc -> Doc`

-- which increases the indentation level for a block of code.

indent :: Doc -> Doc
indent =  local \lvl -> lvl + 1


-- Chapter 11, `Reader` monad exercise 3:
-- (Medium) Use the sequence function defined in Data.Traversable to write a
-- function

--  `cat :: Array Doc -> Doc`

-- which concatenates a collection of documents, separating them with new
-- lines.

cat :: Array Doc -> Doc
cat ds = (joinWith "\n") <$> (sequence (id <$> ds))


-- Chapter 11, `Reader` monad exercise 4:
-- (Medium) Use the `runReader` function to write a function

--  `render :: Doc -> String`

-- which renders a document as a `String`.

-- You should now be able to use your library to write simple documents, as
-- follows:

-- ```
--  render $ cat
--      [ line "Here is some indented text:"
--      , indent $ cat
--          [ line   "I am indented"
--          , line   "So am I"
--          , indent $ line "I am even more indented"
--          ]
--      ]
-- ```

render :: Doc -> String
render doc = runReader doc 0


testRender :: String
testRender =  render $ cat
    [ line "Here is some indented text:"
    , indent $ cat
        [ line   "I am indented"
        , line   "So am I"
        , indent $ line "I am even more indented"
        ]
    ]

--------------------------------------------------------------------------------


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    -- `State` monad -----------------------------------------------------------
    log "11.4.1a - What is the result of replacing `execState` with `runState`?"
    logShow withRun
    log ""

    log "11.4.1b - What is the result of replacing `execState` with `evalState`?"
    logShow withEval
    log ""

    testParensLog

    -- `Reader` monad ----------------------------------------------------------

    log "11.5.4 - Using the `Reader` monad, render an indented document:"
    log ""
    log testRender
    log ""
