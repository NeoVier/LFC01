-- module Tests.Automata exposing (afdTest0)


module Tests.Automata exposing (..)

import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.State as State
import Models.Transition as Transition


stateTest0 : State.State
stateTest0 =
    State.Valid "test0"


stateTest1 : State.State
stateTest1 =
    State.Valid "test1"


stateTest2 : State.State
stateTest2 =
    State.Valid "test2"


stateTest3 : State.State
stateTest3 =
    State.Valid "test3"


stateTest4 : State.State
stateTest4 =
    State.Valid "test4"


symbolTest0 : Alphabet.Symbol
symbolTest0 =
    "a"


symbolTest1 : Alphabet.Symbol
symbolTest1 =
    "b"


alphabetTest : Alphabet.Alphabet
alphabetTest =
    Alphabet.Alphabet [ symbolTest0, symbolTest1 ]


transition0 : Transition.DeterministicTransition
transition0 =
    { prevState = stateTest0
    , nextState = stateTest1
    , conditions = [ symbolTest0 ]
    }


transition1 : Transition.DeterministicTransition
transition1 =
    { prevState = stateTest1
    , nextState = stateTest2
    , conditions = [ symbolTest0 ]
    }


transition2 : Transition.DeterministicTransition
transition2 =
    { prevState = stateTest2
    , nextState = stateTest3
    , conditions = [ symbolTest1 ]
    }


transition3 : Transition.DeterministicTransition
transition3 =
    { prevState = stateTest3
    , nextState = stateTest4
    , conditions = [ symbolTest1 ]
    }


afdTest0 : Automata.AFD
afdTest0 =
    Automata.AFD
        [ stateTest0, stateTest1, stateTest2, stateTest3, stateTest4 ]
        stateTest0
        [ stateTest4 ]
        alphabetTest
        [ transition0, transition1, transition2, transition3 ]
