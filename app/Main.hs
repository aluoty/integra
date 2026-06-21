module Main where

import System.IO

data State = State
    { history :: [String]
    }

main :: IO ()
main = do
    putStrLn "╔════════════════════╗"
    putStrLn "║   Integra v0.1     ║"
    putStrLn "║   Type :help       ║"
    putStrLn "╚════════════════════╝"
    repl (State [])

repl :: State -> IO ()
repl st = do
    putStr "integra> "
    hFlush stdout

    input <- getLine

    case input of
        ":quit" -> do
            putStrLn "Goodbye."

        ":help" -> do
            putStrLn ""
            putStrLn "Commands:"
            putStrLn "  :help      Show help"
            putStrLn "  :history   Show history"
            putStrLn "  :clear     Clear history"
            putStrLn "  :quit      Exit Integra"
            putStrLn ""
            repl st

        ":history" -> do
            putStrLn ""
            mapM_
                (\(i, cmd) -> putStrLn (show i ++ " | " ++ cmd))
                (zip [1 :: Int ..] (history st))
            putStrLn ""
            repl st

        ":clear" -> do
            putStrLn "History cleared."
            repl (State [])

        _ -> do
            case eval input of
                Just result ->
                    putStrLn (show result)
                Nothing ->
                    putStrLn "Invalid expression."

            let newState =
                    st
                        { history = history st ++ [input]
                        }

            repl newState

eval :: String -> Maybe Double
eval str =
    case words str of
        [a] ->
            Just (read a)

        [a, "+", b] ->
            Just (read a + read b)

        [a, "-", b] ->
            Just (read a - read b)

        [a, "*", b] ->
            Just (read a * read b)

        [a, "/", b] ->
            Just (read a / read b)

        _ ->
            Nothing
