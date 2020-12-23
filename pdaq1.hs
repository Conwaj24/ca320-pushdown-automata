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

do_state_transition :: Configuration -> CState -> NState -> Configuration
do_state_transition (state, i, s) cstate nstate | state == cstate = (nstate, i, s)

do_input_transition :: Configuration -> IChar -> Configuration
do_input_transition c "" = c
do_input_transition (st, input, s) ichar | [head input] == ichar = (st, tail input, s)

do_stack_transition :: Configuration -> SChar -> PChar -> Configuration
do_stack_transition (s, i, stack) "" pchar =  (s, i, pchar ++ stack)
do_stack_transition (s, i, stack) schar pchar | [head stack] == schar =  (s, i, pchar ++ tail stack)

do_transition :: Configuration -> Transition -> Configuration
do_transition (state, input, stack) ((cstate, cinput, cstack), (nstate, nstack)) =
   do_stack_transition (
      do_input_transition (
         do_state_transition (state, input, stack) cstate nstate
      ) cinput
   ) cstack nstack

final_configurations :: Configuration -> [Transition] -> [Configuration]
final_configurations (state, "", stack) _ = [(state, "", stack)]
final_configurations c ts = -- to be continued

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

