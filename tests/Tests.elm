module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import KeyedList exposing (..)


conversions : Test
conversions =
    describe "converting to and from Lists"
        [ fuzz (list int) "should restore the original list" <|
            \fuzzList ->
                fuzzList
                    |> fromList
                    |> toList
                    |> Expect.equal fuzzList
        , fuzz (list int) "generating from a List should preserve length" <|
            \fuzzList ->
                fromList fuzzList
                    |> KeyedList.length
                    |> Expect.equal (List.length fuzzList)
        , test "empty Lists should become empty KeyedLists" <|
            \_ ->
                fromList []
                    |> isEmpty
                    |> Expect.true "KeyedList was not empty"
        ]


keyedListFuzzer : Fuzzer (KeyedList Int)
keyedListFuzzer =
    Fuzz.map fromList (list int)


emptyTestFailureMsg : Bool -> Int -> String
emptyTestFailureMsg empty len =
    let
        lengthString =
            len |> toString

        isEmptyQualifier =
            if empty then
                ""
            else
                "not "
    in
        "length == "
            ++ lengthString
            ++ " but list is "
            ++ isEmptyQualifier
            ++ "empty"


empty : Test
empty =
    describe "empty"
        [ fuzz keyedListFuzzer "should be empty iff length = 0" <|
            \fuzzList ->
                let
                    empty =
                        isEmpty fuzzList

                    len =
                        length fuzzList
                in
                    (empty && len == 0)
                        || (not empty && len > 0)
                        |> Expect.true (emptyTestFailureMsg empty len)
        ]


push : Test
push =
    describe "push"
        [ fuzz keyedListFuzzer "should increase length by 1" <|
            \fuzzList ->
                KeyedList.push 9 fuzzList
                    |> KeyedList.length
                    |> Expect.equal (KeyedList.length fuzzList + 1)
        , test "should work on empty KeyedLists" <|
            \_ ->
                KeyedList.push 1 KeyedList.empty
                    |> KeyedList.toList
                    |> Expect.equal [ 1 ]
        , fuzz keyedListFuzzer "should push value onto beginning of list" <|
            \fuzzList ->
                KeyedList.push 9 fuzzList
                    |> toList
                    |> Expect.equal (9 :: toList fuzzList)
        ]


takeKeys : Int -> KeyedList a -> List Key
takeKeys n items =
    let
        keyHelper key _ =
            key
    in
        KeyedList.keyedMap keyHelper items
            |> List.take n


remove : Test
remove =
    describe "remove"
        [ test "should remove items from lists with many items" <|
            \_ ->
                let
                    items =
                        fromList [ 1, 2 ]

                    keys =
                        takeKeys 1 items
                in
                    List.foldl KeyedList.remove items keys
                        |> toList
                        |> List.length
                        |> Expect.equal 1
        , test "should remove items from singleton lists" <|
            \_ ->
                let
                    items =
                        fromList [ 1 ]

                    keys =
                        takeKeys 1 items
                in
                    List.foldl KeyedList.remove items keys
                        |> toList
                        |> Expect.equal []
        , test "should leave empty lists unchanged" <|
            \_ ->
                let
                    keys =
                        takeKeys 1 <| fromList [ 1 ]
                in
                    List.foldl KeyedList.remove KeyedList.empty keys
                        |> toList
                        |> Expect.equal []
        ]


suite : Test
suite =
    describe "KeyedList module"
        [ conversions
        , empty
        , push
        , remove
        ]
