module Lib
    ( someFunc
    , header
    ) where

import qualified Data.Map as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data TapeLetter = Blank | Stroke deriving(Eq,Read,Ord)

instance Show TapeLetter where
    show Stroke = "1"
    show Blank  = "B"

data Action 
    = MoveRight
    | MoveLeft
    | WriteStroke
    | DeleteStroke

type State = Int

data ComputingState 
    = CS{
        state :: State
    ,   left :: [TapeLetter]
    ,   right :: [TapeLetter]
    }

scanLetter :: ComputingState -> TapeLetter
scanLetter (CS _ _ []) = Blank
scanLetter (CS _ _ (x:rt)) = x

data CurrentMachine
    = CM{
        computingState :: ComputingState
    ,   scannedLetter :: TapeLetter
    ,   nowAction :: Action
    }

header :: M.Map (State,TapeLetter) (Action,State) -> ComputingState -> ComputingState
header mp (CS nowState leftTape rightTape) =
    let scannedLetter = if rightTape == [] then Blank else head rightTape
        x = M.lookup (nowState,scannedLetter) mp
    in case x of
            Nothing -> CS nowState leftTape rightTape
            Just (action,nextState) -> case action of
                MoveRight -> case rightTape of
                    [] -> CS nextState (Blank:leftTape) []
                    x:xs -> CS nextState (x:leftTape) xs
                MoveLeft -> case leftTape of
                    [] -> CS nextState [] (Blank:rightTape)
                    x:xs -> CS nextState xs (x:rightTape)
                WriteStroke -> case rightTape of
                    [] -> CS nextState leftTape (Stroke : rightTape)
                    x:xs -> CS nextState leftTape (Stroke : xs)
                DeleteStroke -> case rightTape of
                    [] -> CS nextState leftTape (Blank : rightTape)
                    x:xs -> CS nextState leftTape (Blank : xs)