#!/usr/bin/env ghci

type State = Integer
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

do_state_transition :: Maybe Configuration -> CState -> NState -> Maybe Configuration
do_state_transition Nothing _ _ = Nothing
do_state_transition (Just (state, i, s)) cstate nstate
   | state == cstate = Just (nstate, i, s)
   | otherwise = Nothing

do_input_transition :: Maybe Configuration -> IChar -> Maybe Configuration
do_input_transition Nothing _ = Nothing
do_input_transition c "" = c
do_input_transition (Just (_, "", _)) _ = Nothing
do_input_transition (Just (st, input, s)) ichar
   | [head input] == ichar = Just (st, tail input, s)
   | otherwise = Nothing

do_stack_transition :: Maybe Configuration -> SChar -> PChar -> Maybe Configuration
do_stack_transition Nothing _ _ = Nothing
do_stack_transition (Just (s, i, stack)) "" pchar =  Just (s, i, pchar ++ stack)
do_stack_transition (Just (_, _, "")) _ pchar =  Nothing
do_stack_transition (Just (s, i, stack)) schar pchar
   | [head stack] == schar =  Just (s, i, pchar ++ tail stack)
   | otherwise = Nothing

do_transition :: Configuration -> Transition -> Maybe Configuration
do_transition (state, input, stack) ((cstate, cinput, cstack), (nstate, nstack)) =
   do_stack_transition (
      do_input_transition (
         do_state_transition (Just (state, input, stack)) cstate nstate
      ) cinput
   ) cstack nstack

do_transitions :: Configuration -> [Transition] -> [Configuration]
do_transitions _ [] = []
do_transitions c ts = case do_transition c (head ts) of
                        Nothing -> do_transitions c (tail ts)
                        Just nc -> nc : do_transitions c (tail ts)

final_configurations :: Configuration -> [Transition] -> [Configuration]
final_configurations (state, "", stack) ts | final == [] = [(state, "", stack)]
                                           | otherwise = final
                                           where final = do_transitions (state, "", stack) ts
final_configurations c ts = concat (map (\confs -> final_configurations confs ts) (do_transitions c ts))

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

