module Main (main) where

--------------------------------------------------------------------------
---------------------- Сторонние библиотеки ------------------------------
--------------------------------------------------------------------------
import System.Random
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (try, IOException)
import Control.Monad (replicateM, mapM_)

--------------------------------------------------------------------------
---------------------- Собственные модули --------------------------------
--------------------------------------------------------------------------
import Lib (red, green, blue, white, bold, underline, colorPrint, resetConsole)
--------------------------------------------------------------------------
---------------------- Конструкторы данных -------------------------------
--------------------------------------------------------------------------
data Bounds = Bounds {
        low  :: Int,
        high :: Int
    } deriving (Show, Read)

--------------------------------------------------------------------------
---------------------- Функции -------------------------------------------
--------------------------------------------------------------------------


---------------------- Чистые функции ------------------------------------


-- Выбор одной строки из текста по индексу
returnSingleLine :: String -> Int -> String
returnSingleLine text number | number > amountOfLines =  strings !! number
                             | otherwise = ""
                                where   strings = lines text
                                        amountOfLines = length strings


---------------------- "Грязные" функции ---------------------------------

-- Генерация случайного значения из заданого диапазона
-- randomRIO :: (Random a, MonadIO m) => (a, a) -> m a
getRandomNumber :: Bounds -> IO Int
getRandomNumber (Bounds minVal maxVal) = randomRIO (minVal, maxVal)

-- Получение n случайных строк
genNRandomLinesFromText :: String -> Int -> IO [String]
genNRandomLinesFromText text n =  do
    let allLines = lines text
        amountOfLines = length allLines
        bounds = Bounds 0 (amountOfLines-1)
    -- replicateM :: Applicative m => Int -> m a -> m [a]
    -- n раз выполняет заданное действие и возвращает список результатов
    if amountOfLines >= 1
        then
            replicateM n $ do
                idx <- getRandomNumber bounds
                return (allLines !! idx)
        else
            return []

main :: IO ()
main = do
    colorPrint (bold ++ green) "Enter path to file:" 
    contentsPath <- getLine
    -- Проверка на существование файла по заданному пути
    exists <- doesFileExist contentsPath
    -- Получение содержимого файла:
    --      сначала проверка на то, что по введеному пути сущестует файл
    --      затем - попытка чтения содержимого, возвращается Either, где 
    --              левое значение отвечает за ошибку при чтение, а правое
    --              за прочитанное содержимое
    --       в случае неуспеха (несуществования файла или невозвоможности 
    --              чтения из него) - печатается соответствующее сообщение 
    --              и возвращается пустая строка.
    content <- if exists 
        then do 
            content <- try (readFile contentsPath) :: IO (Either IOException String)
            case content of
                Left ex  -> do
                    colorPrint red ("Ошибка при чтении: " ++ show ex)
                    return ""
                Right c  -> return c
        else do
            colorPrint red ("Файл " ++ contentsPath ++ " не существует.")
            return ""

    -- Генерация случайного значения и получения строки соответсвующей 
    -- этому значению
    let amountOfLines = length $ lines content
        bounds = Bounds 0 (amountOfLines-1)
    randomNum <-getRandomNumber bounds
    let lineOfProphecy = returnSingleLine content randomNum

    colorPrint (bold ++ green) ("Your random number: " 
                                ++ (resetConsole ++ white) ++ show randomNum)
    colorPrint (bold ++ green) ("Your line of prohecy:" 
                                ++ (resetConsole ++ white) ++ lineOfProphecy)


    -- Получение случайного количества случайных предсказаний
    amountOfPropheciesToGet <- getRandomNumber bounds
    prophecies <- genNRandomLinesFromText content amountOfPropheciesToGet
    let countedProphecies = zip [1..] prophecies
    colorPrint (bold ++ green) ("Your number of prophecies: " 
                                ++ (resetConsole ++ white) ++ show amountOfPropheciesToGet)
    colorPrint (bold ++ green) "Your prohecies:" 
    let forPrint (idx, ph) = colorPrint (underline ++ blue) (show idx ++ ")" 
                                                    ++ (resetConsole ++ white) ++ " " ++ ph)
    mapM_ forPrint countedProphecies

    -- Запись в файл 
    let filename = "../output.txt"
    colorPrint (bold ++ green) ("Writing your prophecies to file (" 
                                ++ (resetConsole ++ white)  ++ filename 
                                ++ (bold ++ green) ++"): ")
    -- Записать одну строчку, стерев все что было в файле
    writeFile filename $ head prophecies ++ "\n"
    -- Дописать строчки в файл
    mapM_ (appendFile filename) $ drop 1 prophecies
