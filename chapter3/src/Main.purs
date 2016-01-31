module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.List
import Data.Maybe
import Control.Plus     (empty)

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
showAddress addr = addr.street ++ ", " ++ addr.city ++ ", " ++ addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons
 
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


--- Chapter 3, exercise 2:
--- (Medium) Write a function which looks up an Entry given a street address,
--- by reusing the existing code in findEntry. Test your function in PSCi.

findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = head <<< filter filterEntry
    where
    filterEntry :: Entry -> Boolean
    filterEntry entry =  entry.address.street   == address.street
                      && entry.address.city     == address.city
                      && entry.address.state    == address.state


demoExerciseTwo :: forall e. Eff (console :: CONSOLE | e) Unit
demoExerciseTwo = do
    let a1 = { street: "123 Main St.",  city: "Rockland",   state: "Maine"  }
        a2 = { street: "456 Farm Road", city: "Orono",      state: "Maine"  }
        a3 = { street: "789 No Blvd.",  city: "Portland",   state: "Maine"  }
        e1 = { firstName: "Joe",    lastName: "Schmoe",     address: a1     }
        e2 = { firstName: "John",   lastName: "Doe",        address: a2     }
        book = insertEntry e2 $ insertEntry e1 emptyBook

    log "(\"a1\" and \"a2\" are addresses in \"book\"; \"a3\" is not)\n"

    log "> findEntryByAddress a1 book ="
    print $ showEntry <$> findEntryByAddress a1 book

    log "\n"
    log "> findEntryByAddress a3 book ="
    print $ showEntry <$> findEntryByAddress a3 book


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    demoExerciseTwo
