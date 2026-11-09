{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types 
where

--------------------------------------------------------------------------
------------------------------ Libraries ---------------------------------
--------------------------------------------------------------------------
import Control.Lens.TH
import Data.Default
import qualified Data.Text as T
import Monomer
-- import Data.Semigroup
import TextShow (TextShow)

--------------------------------------------------------------------------
----------------------------- Data types ---------------------------------
--------------------------------------------------------------------------

------------------------------- Model ------------------------------------
data TodoType
  = Home
  | Work
  | Sports
  deriving (Eq, Show, Enum)

data TodoStatus
  = Pending -- В ожидание
  | Done    -- Сделано
  deriving (Eq, Show, Enum)

data Todo = Todo {
  _todoId :: Millisecond,
  _todoType :: TodoType,
  _status :: TodoStatus,
  _description :: T.Text
} deriving (Eq, Show)

-- Определяет конструктор по умолчанию для типа данных с именем Todo в Haskell,
-- используя класс типа Default из библиотеки data-default 
instance Default Todo where
  def = Todo {
    _todoId = 0,
    _todoType = Home,
    _status = Pending,
    _description = ""
  }

data TodoAction
  = TodoNone
  | TodoAdding
  | TodoEditing Int
  | TodoConfirmingDelete Int Todo
  deriving (Eq, Show)

data TodoModel = TodoModel {
  _todos :: [Todo],
  _activeTodo :: Todo,
  _action :: TodoAction
} deriving (Eq, Show)



------------------------------- Events -----------------------------------
data TodoEvt
  = TodoInit
  -- Creation events
  | TodoNew
  | TodoAdd
  -- Edit events  
  | TodoEdit Int Todo
  | TodoSave Int
  -- | TodoTypeChanged
  -- Delete events
  | TodoDeleteBegin Int Todo
  | TodoConfirmDelete Int Todo
  | TodoCancelDelete
  | TodoDelete Int Todo
  -- UI state events
  | TodoShowEdit
  | TodoHideEdit
  | TodoHideEditDone
  | TodoCancel
  deriving (Eq, Show)

makeLenses 'TodoModel
makeLenses 'Todo

todoTypes :: [TodoType]
todoTypes = enumFrom (toEnum 0 :: TodoType)

todoStatuses :: [TodoStatus]
todoStatuses = enumFrom (toEnum 0 :: TodoStatus)

---------------------- Environment and Node ------------------------------

type TodoWenv = WidgetEnv TodoModel TodoEvt
type TodoNode = WidgetNode TodoModel TodoEvt

