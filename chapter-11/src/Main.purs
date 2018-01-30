module Main where

import Prelude
import Control.Monad.Eff          (Eff, foreachE)
import Control.Monad.Eff.Console  (CONSOLE, log, logShow)
import Control.Monad.Reader       (Reader, ask, local, runReader)
import Control.Monad.State        (State, execState, runState, evalState)
import Control.Monad.State.Class  (modify)
import Control.Monad.Writer       (Writer, execWriter, runWriter)
import Control.Monad.Writer.Class (tell)
import Data.Array                 (replicate)
import Data.Foldable              (intercalate, traverse_)
import Data.Int                   (even)
import Data.Monoid.Additive       (Additive(..))
import Data.String                (toCharArray, joinWith)
import Data.Traversable           (sequence)
import Data.Tuple                 (Tuple(..))


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

-- Chapter 11, `Writer` monad exercise 1:
-- (Medium) Rewrite the `sumArray` function above using the `Writer` monad and
-- the `Additive Int` monoid from the purescript-monoid package.

sumArrayWriter :: Array Int -> Int
sumArrayWriter ns =
    let Additive i = execWriter $ traverse_ (\n -> tell $ Additive n) ns
    in  i


-- Chapter 11, `Writer` monad exercise 2:
-- (Medium) The Collatz function is defined on natural numbers n as n / 2 when
-- n is even, and 3 * n + 1 when n is odd. For example, the iterated Collatz
-- sequence starting at 10 is as follows:

--  `10, 5, 16, 8, 4, 2, 1, ...`

-- It is conjectured that the iterated Collatz sequence always reaches 1 after
-- some finite number of applications of the Collatz function.

-- Write a function which uses recursion to calculate how many iterations of
-- the Collatz function are required before the sequence reaches 1.

-- Modify your function to use the Writer monad to log each application of the
-- Collatz function.

nextCollatz :: Int -> Int
nextCollatz i
    | even i    = i / 2
    | otherwise = 3 * i + 1


collatz :: Int -> Int
collatz = f 1
    where f count i | i == 1    = count
                    | otherwise = f (count + 1) (nextCollatz i)


collatz' :: Int -> Writer (Array String) Int
collatz' = f 1
    where f count i = do
            tell [ show i ]
            case i of
                 1         -> pure count
                 otherwise -> f (count + 1) (nextCollatz i)


collatz'log :: Int -> String
collatz'log =  intercalate ", " <<< execWriter <<< collatz'


collatz'count :: Int -> Int
collatz'count i =
    let Tuple i _ = runWriter (collatz' i)
     in i

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

    -- `Writer` monad ----------------------------------------------------------

    log ( "11.6.1 - Using the `Writer` monad and `Additive Int`, rewrite the"
        <> " `sumArray` function:"
        )
    log (    "  `sumArrayWriter [ 10, 20, 30, 40, 50 ]` = "
        <> show (sumArrayWriter [ 10, 20, 30, 40, 50 ])
        )
    log ""

    log "11.6.2 - Implement the \"Collatz\" function using the `Writer` monad:"
    log (  "  `collatz'log 10` = "
        <> collatz'log 10
        )
    log (  "  `collatz' 10` finished in "
        <> show (collatz'count 10)
        <> " iterations"
        )
    log ""
