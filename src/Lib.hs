{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    , header
    ) where

import qualified Data.Map as M
import Data.Yaml(FromJSON,ToJSON,decodeEither)
import GHC.Generics
import Data.Either(fromRight)
import qualified Data.ByteString as B (readFile)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data TapeLetter = Blank | Stroke deriving(Eq,Read,Ord,Generic)

instance FromJSON TapeLetter

instance Show TapeLetter where
    show Stroke = "1"
    show Blank  = "B"

data Action 
    = MoveRight
    | MoveLeft
    | WriteStroke
    | DeleteStroke
    deriving(Show,Generic)

instance FromJSON Action

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

renderCM :: CurrentMachine -> String
renderCM (CM cs sl nact) =
    let 

type Quadruple = M.Map (State,TapeLetter) (Action,State)

initialize :: IO (M.Map (State,TapeLetter) (Action,State))
initialize = do
    xs <- B.readFile "src/input.yaml"
    let dataList = fromRight [] . decodeEither $ xs :: [((State,TapeLetter),(Action,State))]
        mp = M.fromList dataList
    return mp

headerOneStep :: M.Map (State,TapeLetter) (Action,State) -> ComputingState -> ComputingState
headerOneStep mp (CS nowState leftTape rightTape) =
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

calculate :: Quadruple -> [CurrentMachine] -> [CurrentMachine]
calculate mp cms = undefined