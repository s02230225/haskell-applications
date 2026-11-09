{-# LANGUAGE OverloadedStrings #-}

module UI
where

import Monomer
import Control.Lens ( (&), (^.), non, (.~), (?~), At(at), Ixed(ix) )
import qualified Monomer.Lens as L
import qualified Data.Text as T
import TextShow ( TextShow(showt) )
import Data.Default ( Default(def) )

import Types
import Model
import Constants



customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowButton" ?~ grayLight

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowButton" ?~ gray



todoRowKey :: Todo -> T.Text
todoRowKey todo = "todoRow" <> showt (todo ^. todoId)


-- nodeKey :: WidgetNode s e -> Text -> WidgetNode s e
-- Sets the key of the given node.

-- Ключевой компонент, отображающий одну задачу
todoRow :: TodoWenv -> TodoModel -> Int -> Todo -> TodoNode
todoRow wenv model idx t = animRow `nodeKey` todoKey where
  -- Получение цветов
  sectionBg = wenv ^. L.theme . L.sectionColor
  rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
  rowSepColor = gray & L.a .~ 0.5

  todoKey = todoRowKey t
  todoDone = t ^. status == Done
  isLast = idx == length (model ^. todos) - 1

  -- Разный цвет в зависимости от статуса задачи
  (todoBg, todoFg)
    | todoDone = (doneBg, doneFg)
    | otherwise = (pendingBg, pendingFg)

  -- Получение статуса и установка соответствующей надписи в виджете labelS с соответствующими настройками отображения
  todoStatus = labelS (t ^. status)
    -- Стиль, шрифт, размер, выравнивание, цвет, отступы, радиус скругления, цвет заднего фона.
    `styleBasic` [textFont "Medium", textSize 12, textAscender, textColor todoFg, padding 6, paddingH 8, radius 12, bgColor todoBg]

  -- Функция настройки кнопки: задание подписи, действия, а также внешнего вида в зависимости от разных состояний кнопки
  rowButton caption action = button caption action
    -- кнопка в обычном состояние (в квадратных скобках перечислены параметры стиля)
    `styleBasic` [textFont "Remix", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
    -- кнопка при наведение на нее курсора
    `styleHover` [bgColor sectionBg]
    -- кнопка при фокусировки с помощью клавиатуры
    `styleFocus` [bgColor (sectionBg & L.a .~ 0.5)]
    -- кнопка, когда она одновременно и в фокусе и мышь наведена на нее
    `styleFocusHover` [bgColor sectionBg]

  -- вся информация по одной задаче
  todoInfo = hstack [ -- горизонтальный layout
      vstack [ -- вертикальный layout
        labelS (t ^. todoType) `styleBasic` [textSize 12, textColor darkGray], -- метка, отображающая тип задачи (Home | Work | Sport)
        spacer_ [width 5], -- настраиваемый разделитель (указывается ширина для него)
        label (t ^. description) `styleBasic` [textThroughline_ todoDone] -- метка, содержание описание действия, параметр - перечеркивание текста, если задача выполнена
      ],
      filler,
      box_ [alignRight] todoStatus `styleBasic` [width 80], -- настраиваемый контейнер для одного объекта, выровненый по правому краю, содержащий статус задачи
      spacer,
      rowButton remixEdit2Line (TodoEdit idx t), -- кнопка, отображающая "Редактирование" в виде иконки (доступно блогодаря шрифту remix), связанная с событием ToDoEdit
      spacer,
      rowButton remixDeleteBinLine (TodoDeleteBegin idx t) -- кнопка, отображающая "Удаление" в виде иконки (доступно блогодаря шрифту remix), связанная с событием TodoDeleteBegin
    ] `styleBasic` [ paddingV 15, styleIf (not isLast) $ borderB 1 rowSepColor ] -- параметрый всей строки задачи

  -- Анимация: animFadeOut_ для плавного удаления
  animRow = animFadeOut_ [onFinished (TodoDelete idx t)] todoInfo

-- Весь пользовательский интерфейс
buildUI :: TodoWenv -> TodoModel -> TodoNode
buildUI wenv model = widgetTree where
  sectionBg = wenv ^. L.theme . L.sectionColor

  isEditing
    | TodoEditing _ <- model ^. action = True -- показывать слой редактирования, когда идет редактирование
    | TodoAdding <- model ^. action = True    -- или добавление задачи
    | otherwise = False                       -- Иначе, слой должен быть скрыт

  -- Заголовок: счетчик задач
  countLabel = label caption `styleBasic` styles where
    caption = "Tasks (" <> showt (length $ model ^. todos) <> ")"
    styles = [textFont "Regular", textSize 16, padding 20, bgColor sectionBg]

  -- Создание списка задач и расположение их вертикально
  todoList = vstack (zipWith (todoRow wenv model) [0..] (model ^. todos))

  -- Кнопка с основным стилем (mainButton), названием "New" и событием TodoNew; как узел дерева виджетов связывается с именем todoNew
  newButton = mainButton "New task" TodoNew `nodeKey` "todoNew"
    `nodeVisible` not isEditing -- скрыть, если редактируется существующая задача

  -- Слой редактирования задачи
  -- Реализация появления/исчезновения формы редактирования:
  --  Въезжает сверху при открытии
  --  Уезжает вверх при закрытии
  --  Генерирует событие TodoHideEditDone после завершения
  editLayer = content where
    dualSlide content = outer where
      inner = animSlideIn_ [slideTop, duration 200] content
        `nodeKey` "animEditIn"
      outer = animSlideOut_ [slideTop, duration 200, onFinished TodoHideEditDone] inner
        `nodeKey` "animEditOut"

    content = vstack [
        dualSlide (todoEdit wenv model),
        filler
      ] `styleBasic` [bgColor (grayDark & L.a .~ 0.5)]

  -- Всплывающее окно подтверждения удаления задачи
  confirmDeleteLayer = case model ^.action of
    TodoConfirmingDelete idx todo -> [popup] where
      popup = confirmMsg msg (TodoConfirmDelete idx todo) TodoCancelDelete
      msg = "Are you sure you want to delete '" <> (todo ^. description) <> "' ?"
    _ -> []

  -- Основное окно с задачами
  mainLayer = vstack [
      countLabel,
      scroll_ [] (todoList `styleBasic` [padding 20, paddingT 5]), -- окно прокрутки (здесь и располагается список задач)
      filler,
      box_ [alignRight] newButton -- кнопка "New" выровненая по правому краю
        `styleBasic` [bgColor sectionBg, padding 20]
    ]

-- Слои:
--   Основной слой со списком
--   Слой редактирования (появляется поверх)
--   Слой подтверждения удаления (popup)
  -- widgetTree = zstack ([
  --     mainLayer,
  --     editLayer `nodeVisible` isEditing
  --   ] <> confirmDeleteLayer)
  widgetTree = zstack (
    mainLayer : [editLayer | isEditing] -- Добавляем только если нужно
    <> confirmDeleteLayer)

-- Меню редактирования задачи
todoEdit :: TodoWenv -> TodoModel -> TodoNode
todoEdit wenv model = editNode where
  sectionBg = wenv ^. L.theme . L.sectionColor
  isValidInput = model ^. activeTodo . description /= "" -- допустимо только не пустое описание задачи

  -- Кнопка и метка на ней зависят от того, редактируем ли мы задачу или создаем новую
  (saveAction, saveLabel) = case model ^. action of
    TodoEditing idx -> (TodoSave idx, "Save")
    _ -> (TodoAdd, "Add")

  saveTodoBtn = mainButton saveLabel saveAction

  editFields = keystroke [("Enter", saveAction) | isValidInput] $ vstack [
      hstack [
        label "Task:",
        spacer,
        textField (activeTodo . description) `nodeKey` "todoDesc"
      ],
      spacer,
      hgrid [
        hstack [
          label "Type:",
          spacer,
          textDropdownS (activeTodo . todoType) todoTypes `nodeKey` "todoType",
            spacer -- Added here to avoid grid expanding it to 1/3 total width
          ],
        hstack [
          label "Status:",
          spacer,
          textDropdownS (activeTodo . status) todoStatuses
        ]
      ]
    ]

  editNode = keystroke [("Esc", TodoCancel)] $ vstack [
      editFields,
      spacer,
      hstack [
        filler,
        saveTodoBtn `nodeEnabled` isValidInput,
        spacer,
        button "Cancel" TodoCancel
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

------------------------------- Events -----------------------------------
handleEvent
  :: TodoWenv
  -> TodoNode
  -> TodoModel
  -> TodoEvt
  -> [EventResponse TodoModel TodoEvt TodoModel TodoEvt]
handleEvent wenv node model evt = case evt of
  TodoInit -> [SetFocusOnKey "todoNew"]

  TodoNew -> [
    Event TodoShowEdit,
    Model $ model
      & action .~ TodoAdding
      & activeTodo .~ def,
    SetFocusOnKey "todoDesc"]

  TodoEdit idx td -> [
    Event TodoShowEdit,
    Model $ model
      & action .~ TodoEditing idx
      & activeTodo .~ td,
    SetFocusOnKey "todoDesc"]

  TodoAdd -> [
    Event TodoHideEdit,
    Model $ addNewTodo wenv model,
    SetFocusOnKey "todoNew"]

  TodoSave idx -> [
    Event TodoHideEdit,
    Model $ model
      & todos . ix idx .~ (model ^. activeTodo),
    SetFocusOnKey "todoNew"]

  TodoDeleteBegin idx todo -> [
    Model (model & action .~ TodoConfirmingDelete idx todo)]

  TodoConfirmDelete idx todo -> [
    Model (model & action .~ TodoNone),
    Message (WidgetKey (todoRowKey todo)) AnimationStart]

  TodoCancelDelete -> [
    Model (model & action .~ TodoNone)]

  TodoDelete idx todo -> [
    Model $ model & todos .~ remove idx (model ^. todos),
    SetFocusOnKey "todoNew"]

  TodoCancel -> [
    Event TodoHideEdit,
    Model $ model
      & activeTodo .~ def,
    SetFocusOnKey "todoNew"]

  TodoShowEdit -> [
    Message "animEditIn" AnimationStart,
    Message "animEditOut" AnimationStop
    ]

  TodoHideEdit -> [
    Message "animEditIn" AnimationStop,
    Message "animEditOut" AnimationStart
    ]

  TodoHideEditDone -> [
    Model $ model
      & action .~ TodoNone]

addNewTodo :: WidgetEnv s e -> TodoModel -> TodoModel
addNewTodo wenv model = newModel where
  newTodo = model ^. activeTodo
    & todoId .~ currentTimeMs wenv
  newModel = model
    & todos .~ (newTodo : model ^. todos)

