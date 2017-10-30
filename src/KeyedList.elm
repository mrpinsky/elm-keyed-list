module KeyedList
    exposing
        ( KeyedList
        , Key
        , empty
        , cons
        , push
        , fromList
        , decoder
        , toList
        , keyedMap
        , encode
        , isEmpty
        , length
        , update
        , remove
        , filter
        , map
        )


{-| A library for lists of things you want to track, kind of like Html.Keyed but for your data model.


# Keyed Lists

@docs KeyedList, Key


# Build

@docs empty, cons, push, fromList, decoder


# Modify

@docs update, remove


# Consume

@docs toList, keyedMap, encode


# Common Helpers

@docs isEmpty, length


# Transform

@docs filter, map

-}


import Json.Encode as Encode
import Json.Decode as Decode


{-| A list that gives each element a locally unique [`Key`](#Key) for later modification or removal.

Convert a list using [`fromList`](#fromList) or build one from scratch with [`empty`](#empty) and [`push`](#push).

-}
type KeyedList a
    = KeyedList Key (List (Keyed a))


type Keyed a
    = Keyed Key a


{-| An item identifier. Use with [`update`](#update) and [`remove`](#remove).
-}
type Key
    = Key Int


{-| A list with no items.
-}
empty : KeyedList a
empty =
    KeyedList (Key 0) []


{-| Attach a new item to the beginning of the list.
-}
cons : a -> KeyedList a -> KeyedList a
cons item (KeyedList key items) =
    Keyed key item
        :: items
        |> KeyedList (next key)


{-| Attach a new item to the end of the list.
-}
push : a -> KeyedList a -> KeyedList a
push item (KeyedList key items) =
    Keyed key item
        |> List.singleton
        |> (++) items
        |> KeyedList (next key)


{-| Internal utility for push and cons. Gives the next key.
-}
next : Key -> Key
next (Key key) =
    Key (key + 1)


{-| Convert a `List` to a `KeyedList`, preserving the order of the items.
-}
fromList : List a -> KeyedList a
fromList items =
    let
        fromListHelper index item =
            Keyed (Key index) item
    in
        List.indexedMap fromListHelper items
            |> KeyedList (Key <| List.length items)


{-| Update the value of a list for a specific `Key` with a given function.
-}
update : Key -> (a -> a) -> KeyedList a -> KeyedList a
update key fn (KeyedList nextKey items) =
    let
        updateHelper (Keyed k val) =
            if k == key then
                Keyed k (fn val)
            else
                Keyed k val
    in
        List.map updateHelper items
            |> KeyedList nextKey


{-| Remove an item from a list by `Key`. If the `Key` is not found, no changes are made.
-}
remove : Key -> KeyedList a -> KeyedList a
remove key (KeyedList nextKey items) =
    let
        removeHelper (Keyed k _) =
            k /= key
    in
        List.filter removeHelper items
            |> KeyedList nextKey


{-| Convert to a regular `List`.
-}
toList : KeyedList a -> List a
toList (KeyedList _ items) =
    let
        keyedMapHelper (Keyed _ item) =
            item
    in
        List.map keyedMapHelper items


{-| Create a `List` out of items and their `Key`s. This is particularly useful in the `view` of a `Model` that contains a `KeyedList`.

    type alias Model =
        { submodels : KeyedList SubModel
          ...
        }

    view : Model -> Html Msg
    view model =
        keyedMap viewKeyedSubmodel model.submodels
            |> div []

    viewKeyedSubmodel : Key -> SubModel -> Html Msg
    viewKeyedSubmodel key submodel =
        div [ onClick <| Click key ] [ SubModel.view submodel ]

-}
keyedMap : (Key -> a -> b) -> KeyedList a -> List b
keyedMap fn (KeyedList _ items) =
    let
        keyedMapHelper (Keyed key item) =
            fn key item
    in
        List.map keyedMapHelper items


{-| Check if a list is currently empty
-}
isEmpty : KeyedList a -> Bool
isEmpty (KeyedList _ items) =
    List.isEmpty items


{-| Get the current length of a list
-}
length : KeyedList a -> Int
length (KeyedList _ items) =
    List.length items


{-| Apply a function to every item in a list, preserving `Key`s.
-}
map : (a -> b) -> KeyedList a -> KeyedList b
map fn (KeyedList nextKey items) =
    let
        mapHelper (Keyed key item) =
            Keyed key <| fn item
    in
        List.map mapHelper items
            |> KeyedList nextKey


{-| Keep only elements that satisfy the predicate, preserving `Key`s.
-}
filter : (a -> Bool) -> KeyedList a -> KeyedList a
filter predicate (KeyedList nextKey items) =
    let
        filterHelper (Keyed _ item) =
            predicate item
    in
        List.filter filterHelper items
            |> KeyedList nextKey


{-| Encode a `KeyedList` to JSON
-}
encode : (a -> Encode.Value) -> KeyedList a -> Encode.Value
encode encodeItem (KeyedList (Key key) items) =
    Encode.object
        [ ( "nextKey", Encode.int key)
        , ( "items", Encode.list <| List.map (encodeKeyed encodeItem) items )
        ]


encodeKeyed : (a -> Encode.Value) -> Keyed a -> Encode.Value
encodeKeyed encodeItem (Keyed (Key key) item) =
    Encode.object
        [ ( "key", Encode.int key )
        , ( "item", encodeItem item )
        ]


{-| Decode a `KeyedList` from JSON
-}
decoder : (Decode.Decoder a) -> Decode.Decoder (KeyedList a)
decoder itemDecoder =
    Decode.map2
        KeyedList
        (Decode.field "nextKey" keyDecoder)
        (Decode.field "items" <| Decode.list (keyedDecoder itemDecoder))


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map Key Decode.int



keyedDecoder itemDecoder =
    Decode.map2
        Keyed
        (Decode.field "key" keyDecoder)
        (Decode.field "item" itemDecoder)
