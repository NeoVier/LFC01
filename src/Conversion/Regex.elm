module Conversion.Regex exposing (erToAfd)

import Dict exposing (Dict)
import Models.Alphabet as Alphabet
import Models.Automata as Automata
import Models.Regex as Regex exposing (Regex(..))
import Models.State as State
import Models.Transition as Transition
import Utils.Utils as Utils



-- Concat the whole thing with a #
-- Assign nullable to each node
-- Assign firstPos to each node
-- Assign lastPos to each node
-- Assign followPos to each node
-- Construct Dstates and Dtransitions
-- Initialize Dstates with only the first unmarked state of firstpos(root)
--
-- while there is an unmarked state S in Dstates:
--   mark S
--   for each input symbol a:
--      let U be the union of followpos(p) for all p in S that correspond to a
--      if U is not in Dstates:
--          add U as an unmarked state to Dstates
--      Dtran[S, a] = U
--
{-
    The following tables show the relationship between RegEx nodes and their
    nullable, firstPos and lastPos values

    NULLABLE
   +---------------------+------------------------------+
   | Node n              | nullable(n)                  |
   +---------------------+------------------------------+
   +---------------------+------------------------------+
   | Epsilon             | True                         |
   +---------------------+------------------------------+
   | leaf with positon i | False                        |
   +---------------------+------------------------------+
   | c1 | c2             | nullable(c1) || nullable(c2) |
   +---------------------+------------------------------+
   | c1 . c2             | nullable(c1) && nullable(c2) |
   +---------------------+------------------------------+
   | (c1)*               | True                         |
   +---------------------+------------------------------+

    FIRST POS
   +---------------------+-----------------------------------------------------+
   | Node n              | firstPos(n)                                         |
   +---------------------+-----------------------------------------------------+
   +---------------------+-----------------------------------------------------+
   | Epsilon             | {}                                                  |
   +---------------------+-----------------------------------------------------+
   | leaf with positon i | {i}                                                 |
   +---------------------+-----------------------------------------------------+
   | c1 | c2             | firstPos(c1) ++ firstPos(c2)                        |
   +---------------------+-----------------------------------------------------+
   | c1 . c2             | if (nullable(c1)) then firstPos(c1) ++ firstPos(c2) |
   |                     |   else firstPos(c1)                                 |
   +---------------------+-----------------------------------------------------+
   | (c1)*               | firstPos(c1)                                        |
   +---------------------+-----------------------------------------------------+


    LAST POS
   +---------------------+-----------------------------------------------------+
   | Node n              | lastPos(n)                                          |
   +---------------------+-----------------------------------------------------+
   +---------------------+-----------------------------------------------------+
   | Epsilon             | {}                                                  |
   +---------------------+-----------------------------------------------------+
   | leaf with positon i | {i}                                                 |
   +---------------------+-----------------------------------------------------+
   | c1 | c2             | lastPos(c1) ++ lastPos(c2)                          |
   +---------------------+-----------------------------------------------------+
   | c1 . c2             | if (nullable(c2)) then lastPos(c1) ++ lastPos(c2)   |
   |                     |   else lastPos(c2)                                  |
   +---------------------+-----------------------------------------------------+
   | (c1)*               | lastPos(c1)                                         |
   +---------------------+-----------------------------------------------------+


    FOLLOW POS
    - If n is a (Concat c1 c2) node, for each position i in lastpos(c1), all
        positions in firstpos(c2) are in followpos(i)
    - If n is a (Star c1) node, and i is a position in lastpos(n), then all
        positions in firstpos(n) are in followpos(i)
-}


type alias TreeInfo =
    { nullable : Bool
    , firstPos : List Int
    , lastPos : List Int
    , followPos : Maybe (List Int)
    , nodeSymbol : Maybe Alphabet.Symbol
    }


type alias TreeInfoDict =
    Dict Int TreeInfo


treeInfoDict : Regex -> TreeInfoDict
treeInfoDict r =
    nullableDict r Dict.empty
        |> firstPosDict r
        |> lastPosDict r
        |> followPosDict r



-- |> cleanTreeInfoDict


emptyTreeInfo : TreeInfo
emptyTreeInfo =
    { nullable = False
    , firstPos = []
    , lastPos = []
    , followPos = Nothing
    , nodeSymbol = Nothing
    }


cleanTreeInfoDict : TreeInfoDict -> TreeInfoDict
cleanTreeInfoDict infoTree =
    Dict.filter (\_ v -> v.nodeSymbol /= Nothing) infoTree
        |> Dict.toList
        |> List.map (\( k, v ) -> v)
        |> (\x ->
                List.map2 (\idx v -> ( idx, v ))
                    (List.range 1 (List.length x))
                    x
           )
        |> Dict.fromList



-------
-- Get root node
--
-- Get its firstPos
--
-- For each item in the firstPos:
--      Get what each component's nodeSymbol is
--
--
-- If first pos = [1, 2, 3]
--    and treeInfoDict[1].nodeSymbol = treeInfoDict[3].nodeSymbol = a
--
-- To transition from [1,2,3] through a,
--    go to state treeInfoDict[1].followPos ++ treeInfoDict[3].followPos
--    (e.g. [1,2,3,4])
-- To transition from [1,2,3] through b,
--    go to state treeInfoDict[3].followPos (e.g. [1,2,3])
--
-- Now do the same with the new states added ([1,2,3,4])
--
--
-- Every state that contains the root node's finalPos is an accepting state
-------


erToAfd : Regex -> Automata.AFD
erToAfd r =
    let
        newR =
            Concat r (Symbol (Alphabet.Single '#'))

        startingAfd =
            { states = []
            , initialState = State.Dead
            , finalStates = []
            , alphabet = []
            , transitions = []
            }

        dirtyInfoDict =
            treeInfoDict newR

        rootNodeInfo =
            Dict.values dirtyInfoDict
                |> Utils.last
    in
    case rootNodeInfo of
        -- Impossible
        Nothing ->
            startingAfd

        Just info ->
            let
                startingState =
                    List.map String.fromInt info.firstPos
                        |> String.join ""
                        |> State.Valid
            in
            erToAfdHelp (cleanTreeInfoDict dirtyInfoDict)
                [ info.firstPos ]
                []
                { startingAfd | initialState = startingState }


erToAfdHelp :
    TreeInfoDict
    -> List (List Int)
    -> List (List Int)
    -> Automata.AFD
    -> Automata.AFD
erToAfdHelp treeInfo statesToCreate seenStates partialAfd =
    case List.head statesToCreate of
        Nothing ->
            partialAfd

        Just newStateComponents ->
            let
                symbolGroups =
                    getSymbolGroups treeInfo newStateComponents

                createState components =
                    List.map String.fromInt components
                        |> String.join ""
                        |> State.Valid

                newState =
                    createState newStateComponents

                isFinal =
                    Dict.keys symbolGroups
                        |> List.member "#"

                newStatesComponents =
                    Dict.values symbolGroups

                newTransitions =
                    Dict.toList symbolGroups
                        |> List.filterMap
                            (\( s, v ) ->
                                if s == "#" then
                                    Nothing

                                else
                                    Just
                                        { prevState = newState
                                        , nextState = createState v
                                        , conditions =
                                            case Utils.stringToSymbol s of
                                                Nothing ->
                                                    []

                                                Just c ->
                                                    [ c ]
                                        }
                            )

                newSymbolsStr =
                    List.filter
                        (\s ->
                            case Utils.stringToSymbol s of
                                Nothing ->
                                    False

                                Just symb ->
                                    not
                                        (List.member symb
                                            partialAfd.alphabet
                                        )
                                        && s
                                        /= "#"
                        )
                        (Dict.keys symbolGroups)

                newSymbols =
                    List.map Utils.stringToSymbol newSymbolsStr
                        |> Utils.listOfMaybesToMaybeList
                        |> Maybe.withDefault []
            in
            if
                List.member newStateComponents seenStates
                    || List.isEmpty newStateComponents
            then
                erToAfdHelp treeInfo
                    (List.drop 1 statesToCreate)
                    seenStates
                    partialAfd

            else
                erToAfdHelp treeInfo
                    (List.drop 1 statesToCreate ++ newStatesComponents)
                    (newStateComponents :: seenStates)
                    { partialAfd
                        | states = partialAfd.states ++ [ newState ]
                        , finalStates =
                            if isFinal then
                                partialAfd.finalStates ++ [ newState ]

                            else
                                partialAfd.finalStates
                        , alphabet =
                            partialAfd.alphabet
                                ++ newSymbols
                        , transitions = partialAfd.transitions ++ newTransitions
                    }


getSymbolGroups : TreeInfoDict -> List Int -> Dict String (List Int)
getSymbolGroups treeInfo indexes =
    List.foldl
        (\index acc ->
            case Dict.get index treeInfo of
                Nothing ->
                    acc

                Just info ->
                    case info.nodeSymbol of
                        Nothing ->
                            acc

                        Just symbol ->
                            case info.followPos of
                                Nothing ->
                                    acc

                                -- TODO
                                Just fp ->
                                    Dict.update (Utils.symbolToString symbol)
                                        (\dictValue ->
                                            case dictValue of
                                                Nothing ->
                                                    Just fp

                                                Just v ->
                                                    Just <| v ++ fp
                                        )
                                        acc
        )
        Dict.empty
        indexes


nullableDict : Regex -> TreeInfoDict -> TreeInfoDict
nullableDict r d =
    let
        idx dict =
            Dict.keys dict |> List.maximum |> Maybe.withDefault 0 |> (+) 1

        insert v dict =
            Dict.insert (idx dict)
                { emptyTreeInfo
                    | nullable = v
                    , nodeSymbol =
                        case r of
                            Symbol s ->
                                Just s

                            _ ->
                                Nothing
                }
                dict

        compose1 c1 =
            insert (nullable r) (nullableDict c1 d)

        compose2 c1 c2 =
            let
                c1Result =
                    nullableDict c1 d

                c2Result =
                    nullableDict c2 c1Result
            in
            insert (nullable r) c2Result
    in
    case r of
        Union c1 c2 ->
            compose2 c1 c2

        Concat c1 c2 ->
            compose2 c1 c2

        Star c1 ->
            compose1 c1

        Plus c1 ->
            compose1 c1

        Question c1 ->
            compose1 c1

        _ ->
            insert (nullable r) d


firstPosDict : Regex -> TreeInfoDict -> TreeInfoDict
firstPosDict r d =
    let
        idx dict =
            Dict.filter (\_ v -> List.isEmpty v.firstPos) dict
                |> Dict.keys
                |> List.head
                |> Maybe.withDefault 0

        update v dict =
            Dict.update (idx dict)
                (Maybe.map (\info -> { info | firstPos = v }))
                dict

        prevMaxValue dict =
            Dict.values dict
                |> List.map .firstPos
                |> List.concat
                |> List.maximum
                |> Maybe.withDefault 0

        compose1 c1 =
            update (firstPos r) (firstPosDict c1 d)

        compose2 c1 c2 =
            let
                c1Result =
                    firstPosDict c1 d

                c2Result =
                    firstPosDict c2 c1Result
            in
            update (firstPos r) c2Result
    in
    case r of
        Union c1 c2 ->
            compose2 c1 c2

        Concat c1 c2 ->
            compose2 c1 c2

        Star c1 ->
            compose1 c1

        Plus c1 ->
            compose1 c1

        Question c1 ->
            compose1 c1

        _ ->
            case firstPosHelp (prevMaxValue d) r of
                ( v, _ ) ->
                    update v d


lastPosDict : Regex -> TreeInfoDict -> TreeInfoDict
lastPosDict r d =
    let
        idx dict =
            Dict.filter (\_ v -> List.isEmpty v.lastPos) dict
                |> Dict.keys
                |> List.head
                |> Maybe.withDefault 0

        update v dict =
            Dict.update (idx dict)
                (Maybe.map (\info -> { info | lastPos = v }))
                dict

        prevMaxValue dict =
            Dict.values dict
                |> List.map .lastPos
                |> List.concat
                |> List.maximum
                |> Maybe.withDefault 0

        compose1 c1 =
            update (lastPos r) (lastPosDict c1 d)

        compose2 c1 c2 =
            let
                c1Result =
                    lastPosDict c1 d

                c2Result =
                    lastPosDict c2 c1Result
            in
            update (lastPos r) c2Result
    in
    case r of
        Union c1 c2 ->
            compose2 c1 c2

        Concat c1 c2 ->
            compose2 c1 c2

        Star c1 ->
            compose1 c1

        Plus c1 ->
            compose1 c1

        Question c1 ->
            compose1 c1

        _ ->
            case lastPosHelp (prevMaxValue d) r of
                ( v, _ ) ->
                    update v d


followPosDict : Regex -> TreeInfoDict -> TreeInfoDict
followPosDict r d =
    let
        idx dict =
            Dict.filter (\_ v -> v.followPos == Nothing) dict
                |> Dict.keys
                |> List.head
                |> Maybe.withDefault 0

        update v dict =
            Dict.update (idx dict)
                (Maybe.map (\info -> { info | followPos = v }))
                dict

        findSymbolIndex s dict =
            Dict.filter (\_ v -> v.firstPos == [ s ] && v.nodeSymbol /= Nothing)
                dict
                |> Dict.keys
                |> List.head
                |> Maybe.withDefault 0
    in
    case r of
        Star c1 ->
            let
                c1Result =
                    followPosDict c1 d

                updateFollowPos info newFollowPos =
                    { info
                        | followPos =
                            case info.followPos of
                                Nothing ->
                                    Just newFollowPos

                                Just v ->
                                    Just <| v ++ newFollowPos
                    }
            in
            case Dict.get (idx c1Result) c1Result of
                -- Impossible
                Nothing ->
                    d

                Just treeInfo ->
                    List.foldl
                        (\lp acc ->
                            Dict.update (findSymbolIndex lp acc)
                                (Maybe.map
                                    (\info ->
                                        updateFollowPos info treeInfo.firstPos
                                    )
                                )
                                acc
                        )
                        c1Result
                        treeInfo.lastPos
                        |> update (Just [])

        Concat c1 c2 ->
            let
                c1Result =
                    followPosDict c1 d

                c1Index =
                    idx c1Result - 1

                c2Result =
                    followPosDict c2 c1Result

                c2Index =
                    idx c2Result - 1

                updateFollowPos info newFollowPos =
                    { info
                        | followPos =
                            case info.followPos of
                                Nothing ->
                                    Just newFollowPos

                                Just v ->
                                    Just <| v ++ newFollowPos
                    }
            in
            case Dict.get c1Index c2Result of
                -- Impossible
                Nothing ->
                    d

                Just c1Value ->
                    case Dict.get c2Index c2Result of
                        -- Impossible
                        Nothing ->
                            d

                        Just c2Value ->
                            List.foldl
                                (\lp acc ->
                                    Dict.update (findSymbolIndex lp acc)
                                        (Maybe.map
                                            (\info ->
                                                updateFollowPos info
                                                    c2Value.firstPos
                                            )
                                        )
                                        acc
                                )
                                c2Result
                                c1Value.lastPos
                                |> update (Just [])

        Union c1 c2 ->
            let
                c1Result =
                    followPosDict c1 d

                c2Result =
                    followPosDict c2 c1Result
            in
            update (Just []) c2Result

        Plus c1 ->
            let
                c1Result =
                    followPosDict c1 d

                updateFollowPos info newFollowPos =
                    { info
                        | followPos =
                            case info.followPos of
                                Nothing ->
                                    Just newFollowPos

                                Just v ->
                                    Just <| v ++ newFollowPos
                    }
            in
            case Dict.get (idx c1Result) c1Result of
                -- Impossible
                Nothing ->
                    d

                Just treeInfo ->
                    List.foldl
                        (\lp acc ->
                            Dict.update (findSymbolIndex lp acc)
                                (Maybe.map
                                    (\info ->
                                        updateFollowPos info treeInfo.firstPos
                                    )
                                )
                                acc
                        )
                        c1Result
                        treeInfo.lastPos
                        |> update (Just [])

        Question c1 ->
            update (Just []) (followPosDict c1 d)

        _ ->
            update (Just []) d


nullable : Regex -> Bool
nullable r =
    case r of
        Epsilon ->
            True

        Symbol _ ->
            False

        Union c1 c2 ->
            nullable c1 || nullable c2

        Concat c1 c2 ->
            nullable c1 && nullable c2

        Star _ ->
            True

        Plus _ ->
            False

        Question _ ->
            True


firstPos : Regex -> List Int
firstPos r =
    firstPosHelp 0 r
        |> Tuple.first


firstPosHelp : Int -> Regex -> ( List Int, Int )
firstPosHelp count r =
    case r of
        Epsilon ->
            ( [], count )

        Symbol _ ->
            ( [ count + 1 ], count + 1 )

        Union c1 c2 ->
            let
                ( c1FirstPos, c1Count ) =
                    firstPosHelp count c1

                ( c2FirstPos, c2Count ) =
                    firstPosHelp c1Count c2
            in
            ( c1FirstPos ++ c2FirstPos, c2Count )

        Concat c1 c2 ->
            let
                ( c1firstPos, c1Count ) =
                    firstPosHelp count c1
            in
            if nullable c1 then
                let
                    ( c2firstPos, c2Count ) =
                        firstPosHelp c1Count c2
                in
                ( c1firstPos ++ c2firstPos, c2Count )

            else
                firstPosHelp count c1

        Star c1 ->
            firstPosHelp count c1

        Plus c1 ->
            firstPosHelp count c1

        Question c1 ->
            firstPosHelp count c1


lastPos : Regex -> List Int
lastPos r =
    let
        v =
            lastPosHelp 0 r |> Tuple.first

        leafCount =
            getLeafCount r
    in
    List.map (\x -> leafCount - x + 1) v |> List.sort


lastPosHelp : Int -> Regex -> ( List Int, Int )
lastPosHelp count r =
    case r of
        Epsilon ->
            ( [], count )

        Symbol _ ->
            ( [ count + 1 ], count + 1 )

        Union c1 c2 ->
            let
                ( c1LastPos, c1Count ) =
                    lastPosHelp count c1

                ( c2LastPos, c2Count ) =
                    lastPosHelp c1Count c2
            in
            ( c1LastPos ++ c2LastPos, c2Count )

        Concat c1 c2 ->
            if nullable c2 then
                let
                    ( c1LastPos, c1Count ) =
                        lastPosHelp count c1

                    ( c2LastPos, c2Count ) =
                        lastPosHelp c1Count c2
                in
                ( c1LastPos ++ c2LastPos, c2Count )

            else
                lastPosHelp count c2

        Star c1 ->
            lastPosHelp count c1

        Plus c1 ->
            lastPosHelp count c1

        Question c1 ->
            lastPosHelp count c1


getLeafCount : Regex -> Int
getLeafCount r =
    case r of
        Epsilon ->
            1

        Symbol _ ->
            1

        Union c1 c2 ->
            getLeafCount c1 + getLeafCount c2

        Concat c1 c2 ->
            getLeafCount c1 + getLeafCount c2

        Star c1 ->
            getLeafCount c1

        Plus c1 ->
            getLeafCount c1

        Question c1 ->
            getLeafCount c1
