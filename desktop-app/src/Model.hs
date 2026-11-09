{-# LANGUAGE OverloadedStrings #-}
module Model
where

import Control.Lens ( (&), (.~) )

--------------------------------------------------------------------------
----------------------------- Data types ---------------------------------
--------------------------------------------------------------------------
import Types
------------------------------- Model ------------------------------------


remove :: Int -> [a] -> [a]
remove idx ls = take idx ls ++ drop (idx + 1) ls

initialTodos :: [Todo]
initialTodos = todos where
  items = mconcat $ replicate 1 [
    Todo 0 Home Done  "Tidy up the room",
    Todo 0 Home Pending "Buy groceries",
    Todo 0 Home Pending "Pay the bills",
    Todo 0 Home Pending "Repair kitchen sink",
    Todo 0 Work Done "Check the status of project A",
    Todo 0 Work Pending "Finish project B",
    Todo 0 Work Pending "Send email to clients",
    Todo 0 Work Pending "Contact cloud services provider"
    ]
  todos = zipWith (\t idx -> t & todoId .~ idx) items [0..]