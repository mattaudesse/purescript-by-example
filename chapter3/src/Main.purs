module Main where

import Prelude
import Control.Monad.Eff         (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.List                 (List(..), filter, head, intercalate)
import Data.Maybe                (Maybe)
import Control.Plus              (empty)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress a = intercalate ", " [ a.street, a.city, a.state ]

showEntry :: Entry -> String
showEntry e =
       e.lastName
    <> ", "
    <> e.firstName
    <> ": "
    <> showAddress e.address

emptyBook :: AddressBook
emptyBook =  empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry =  Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter f
  where f e =  e.firstName == firstName
            && e.lastName  == lastName


--- Chapter 3, exercise 2:
--- (Medium) Write a function which looks up an Entry given a street address,
--- by reusing the existing code in findEntry. Test your function in PSCi.

findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = head <<< filter f
    where f e =  e.address.street == address.street
              && e.address.city   == address.city
              && e.address.state  == address.state


demoExerciseTwo :: forall e. Eff (console :: CONSOLE | e) Unit
demoExerciseTwo = do
    let a1 = { street:    "123 Main St.",  city:     "Rockland", state:   "Maine" }
        a2 = { street:    "456 Farm Road", city:     "Orono",    state:   "Maine" }
        a3 = { street:    "789 No Blvd.",  city:     "Portland", state:   "Maine" }
        e1 = { firstName: "Joe",           lastName: "Schmoe",   address: a1      }
        e2 = { firstName: "John",          lastName: "Doe",      address: a2      }
        book = insertEntry e2 $ insertEntry e1 emptyBook

    log "(\"a1\" and \"a2\" are addresses in \"book\"; \"a3\" is not)\n"

    log "> findEntryByAddress a1 book ="
    logShow $ showEntry <$> findEntryByAddress a1 book

    log "\n"
    log "> findEntryByAddress a3 book ="
    logShow $ showEntry <$> findEntryByAddress a3 book


main :: forall e. Eff (console :: CONSOLE | e) Unit
main =  demoExerciseTwo
