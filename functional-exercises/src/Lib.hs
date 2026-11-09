module Lib
    ( resetConsole
    , black
    , red
    , green
    , yellow
    , blue
    , magenta
    , cyan
    , white
    , bgRed
    , bgGreen
    , bgYellow
    , bgBlue
    , bold
    , underline
    ------------
    , colorPrint
    ) where

-- ANSI color codes
resetConsole :: String
resetConsole = "\ESC[0m"

-- Text colors
black :: String
black = "\ESC[30m"

red :: String
red = "\ESC[31m"

green :: String
green = "\ESC[32m"

yellow :: String
yellow = "\ESC[33m"

blue :: String
blue = "\ESC[34m"

magenta :: String
magenta = "\ESC[35m"

cyan :: String
cyan = "\ESC[36m"

white :: String
white = "\ESC[37m"

-- Background colors
bgRed :: String
bgRed = "\ESC[41m"

bgGreen :: String
bgGreen = "\ESC[42m"

bgYellow :: String
bgYellow = "\ESC[43m"

bgBlue :: String
bgBlue = "\ESC[44m"

-- Text styles
bold :: String
bold = "\ESC[1m"

underline :: String
underline = "\ESC[4m"

-- Helper function for colored output
colorPrint :: String -> String -> IO ()
colorPrint color text = putStrLn $ color ++ text ++ resetConsole
