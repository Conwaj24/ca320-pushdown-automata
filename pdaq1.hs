#!/usr/bin/env ghci

type State = Int
type IState = State -- Initial state
type AState = State -- Accepted state
type CState = State -- Current state
type NState = State -- New state
type PChar = String -- Push char
type IChar = String -- Input char
type SChar = String -- Stack char

type PDA = (IState, [AState], [Transition])

type Transition = (Criteria, Change)
type Criteria = (CState, IChar, SChar)
type Change = (NState, PChar)

type Configuration = (CState, Input, Stack)
type Input = String
type Stack = String

data Result = Accept | Reject deriving (Show, Enum)
instance Eq Result where
   x == y = fromEnum x == fromEnum y

final_configurations :: Configuration -> [Transition] -> [Configuration]
final_configurations (state, "", stack) _ = [(state, "", stack)]
--final_configurations (state, input, stack) transitions = -- to be continued

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains l s = head l == s || contains (tail l) s

configuration_result :: Configuration -> [AState] -> Result
configuration_result (state, "", "") accept_states | contains accept_states state = Accept
                                                   | otherwise = Reject
configuration_result _ _ = Reject

accepted :: [Result] -> Result
accepted results | contains results Accept = Accept
                 | otherwise = Reject

run :: PDA -> Input -> Result
run (i, a, t) s = accepted (map (\confs -> configuration_result confs a) (final_configurations (i, s, "") t))

