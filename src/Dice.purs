-- Copyright 2016 Moritz Schulte <mtesseract@silverratio.net>
-- License: BSD3.

module App.Dice where

import Prelude (($), map, const, show, bind, return,
                (+), (*), (/), (==), (++), (-))
import Pux (EffModel)
import Pux.Html (Html, div, text, button, table, tr, td)
import Pux.Html.Attributes (className)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Class (liftEff)
import Pux.Html.Events (onClick)
import Data.Map (Map, lookup, toList, alter, empty)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Data.Array ((..))
import Text.Format (precision, format)
import Math (abs, round, pow)
import Data.Traversable (sequence)
import Control.Monad.Eff (Eff)

data Action = Reset
            | ThrowDice
            | ThrowDiceMany Int
            | RegisterDice (Array Int)

type Statistics = Map Int Int              
type State = { statistics :: Statistics }

type RelFreq = Map Int Number    

init :: State
init = { statistics: empty }

update :: Action -> State -> EffModel State Action (random :: RANDOM)
update ThrowDice state =
  { state: state
  , effects: [do dice <- liftEff $ randomInt 1 6
                 return $ RegisterDice [dice]]
  }
update (ThrowDiceMany times) state =
  { state: state
  , effects: [do dices <- liftEff $ randomInts 1 6 times
                 return $ RegisterDice dices]
  }
update (RegisterDice ns) state =
  { state: registerDice ns state
  , effects: []
  }
update Reset _ =
  { state: init
  , effects: []
  }

randomInts :: forall e. Int -> Int -> Int -> Eff (random :: RANDOM | e) (Array Int)
randomInts lb ub many =
  sequence $ map (const (randomInt lb ub)) (1 .. many)
    
registerDice :: Array Int -> State -> State
registerDice dices state =
    foldr (\ d s -> s { statistics = alter inc d s.statistics }) state dices
    where inc m = Just $ 1 + fromMaybe 0 m

computeMeanVal :: State -> Maybe Number
computeMeanVal state =
    let stats = toList state.statistics
        s = foldr (\ (Tuple result counter) accu ->
                       { sum: accu.sum + result * counter,
                         total: accu.total + counter})
              { sum: 0, total: 0} stats
    in if s.total == 0
       then Nothing
       else Just $ toNumber s.sum / toNumber s.total

computeTotal :: Statistics -> Int
computeTotal stats =
    foldr (\ (Tuple _ counter) accu -> accu + counter) 0 $ toList stats

computeRelFreq :: State -> Maybe RelFreq
computeRelFreq state =
    let stats = state.statistics
        total = computeTotal stats
    in if total == 0
       then Nothing
       else Just $ map (\ v -> toNumber v / toNumber total) state.statistics

view :: State -> Html Action
view state = do
  let stats = state.statistics
      maybeMeanVal = computeMeanVal state
      total = computeTotal stats
  div
    []
    [ div
        [className "controls"]
        [ div
            [ className "button" ]
            [ button [ onClick (const ThrowDice) ] [ text "1x würfeln" ] ]
        , div
            [ className "button" ]
            [ button [ onClick (const (ThrowDiceMany 10)) ] [ text "10x würfeln" ] ]
        , div
            [ className "button" ]
            [ button [ onClick (const (ThrowDiceMany 100)) ] [ text "100x würfeln" ] ]
        , div
            [ className "button"]
            [ button [ onClick (const (ThrowDiceMany 1000)) ] [ text "1000x würfeln" ] ]
        , div
            [ className "button"]
            [ button [ onClick (const Reset) ] [ text "reset" ] ]
        , div
            [ className "floatClear" ]
            [ ]
        ]
    , div
        []
        [ table
            [className "alternating"]
            ([ tr [] [ td [className "label"] [ text "Augenzahl" ]
                     , td [className "label"] [ text "abs. Häufigkeit" ]
                     , td [className "label"] [ text "rel. Häufigkeit" ]
                     , td [className "label"] [ text "Wahrscheinlichkeit" ]
                     , td [className "label"] [ text "Abweichung" ] ] ]
             ++ map (statRow state) (1..6))
        ]
    , div
        []
        [ table
            [className "summary"]
            [ tr
                []
                [ td [className "label"] [ text "Anzahl Würfe:" ]
                , td [] [ text (show total) ]
                ]
            , tr
                []
                [ td [className "label"] [ text "Erwartungswert:" ]
                , td [] [ text "3.5" ]
                ]
             , tr
                []
                [ td [className "label"] [ text "Mittelwert:" ]
                , td [] [ text (case maybeMeanVal of
                                  Just meanVal -> formatNumber meanVal
                                  Nothing -> "—") ]
                ]
             , tr
                []
                [ td [className "label"] [ text "Abweichung:" ]
                , td [] [ text (case maybeMeanVal of
                                  Just meanVal -> show (roundNum (abs (meanVal - diceMean)))
                                  Nothing -> "—") ]
                ]
            ]
        ]
    ]

diceMean :: Number
diceMean = 3.5

roundNum :: Number -> Number
roundNum x =
    let powerOfTen = pow 10.0 (toNumber accuracy)
    in (round (x * powerOfTen)) / powerOfTen

absFreq :: State -> Int -> Int
absFreq state n =
  let stats = state.statistics
  in fromMaybe 0 $ lookup n stats

-- | Workaround for bug in purescript-format.
formatNumber :: Number -> String
formatNumber n =
    if n == 0.0
    then "0.00"
    else format (precision accuracy) n

diceProbability :: Number
diceProbability = 0.167

accuracy :: Int
accuracy = 3

statRow :: forall b. State -> Int -> Html b
statRow state n = do
  let stats = state.statistics
      maybeRelFreqMap = computeRelFreq state
      nRelFreq = case maybeRelFreqMap of
                   Just relFreqMap -> Just $ fromMaybe 0.0 $ lookup n relFreqMap
                   Nothing -> Nothing
  tr [] [ td [ className "label" ] [ text (show n) ]
        , td [] [ text (show (absFreq state n)) ]
        , td [] [ text (maybe "—" (\ x -> formatNumber x ++ "%") nRelFreq) ]
        , td [] [ text (formatNumber diceProbability) ]
        , td [] [ text (maybe "—" (\ x -> show (roundNum (abs (x - diceProbability)))) nRelFreq) ]
        ]
