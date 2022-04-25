{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}
module Lib
    ( someFunc
    , oneStepCompute
    ) where

import qualified Data.Map as M
import Data.Yaml(FromJSON,ToJSON,decodeEither)
import GHC.Generics
import Data.Either(fromRight)
import qualified Data.ByteString as B (readFile)
import qualified Data.Text as T
import Data.String(IsString)


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
    deriving(Generic)

instance Show Action where
    show MoveRight    = "R"
    show MoveLeft     = "L"
    show WriteStroke  = "S_1"
    show DeleteStroke = "S_0"

instance FromJSON Action

type State = Int

data ComputingState 
    = CS{
        state :: State
    ,   left :: [TapeLetter]
    ,   right :: [TapeLetter]
    }

renderCS :: ComputingState -> T.Text
renderCS (CS st lt rt) = 
    case rt of
        []   -> T.pack . concatMap show $ lt
        x:xs -> let state = "_{" <> T.pack (show st) <> "}"
                    textify = T.pack . show
                    rightTape = textify x : state : map textify xs
                    mergeTapes ys zs = case ys of
                        [] -> zs
                        w:ws -> mergeTapes ws (w:zs)
                    leftTape = map textify lt
                    tape = mergeTapes leftTape rightTape
                in mconcat tape

scanLetter :: ComputingState -> TapeLetter
scanLetter (CS _ _ []) = Blank
scanLetter (CS _ _ (x:rt)) = x

data CurrentMachine
    = CM{
        computingState :: ComputingState
    ,   scannedLetter :: TapeLetter
    ,   nowAction :: Maybe Action
    }

renderCM :: CurrentMachine -> T.Text
renderCM (CM cs sl nact) = 
    let computingstate = "\\UnaryInfC{" <> renderCS cs <> "}\n"
    in case nact of
        Nothing -> computingstate
        Just act -> let 
                        textify = T.pack . show
                        label = "\\RightLabel{" <> textify sl <> "\\colon " <> T.pack (show act) <> "}\n"
                    in computingstate <> label
                    

type Quadruple = M.Map (State,TapeLetter) (Action,State)

initialize :: IO (M.Map (State,TapeLetter) (Action,State))
initialize = do
    xs <- B.readFile "src/input.yaml"
    let dataList = fromRight [] . decodeEither $ xs :: [((State,TapeLetter),(Action,State))]
        mp = M.fromList dataList
    return mp

oneStepCompute :: M.Map (State,TapeLetter) (Action,State) -> ComputingState -> ComputingState
oneStepCompute mp (CS nowState leftTape rightTape) =
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