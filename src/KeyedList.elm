module KeyedList
    exposing
        ( KeyedList
        , Key
        , empty
        , cons
        , push
        , fromList
        , toList
        , keyedMap
        , update
        , remove
        , filter
        , map
        )

{-| A library for lists of things you want to track, kind of like Html.Keyed but for your data model.


# Keyed Lists

@docs KeyedList, Key


# Build

@docs empty, cons, push, fromList


# Consume

@docs toList, keyedMap


# Modify

@docs update, remove


# Transform

@docs filter, map

-}


type KeyedList a
    = KeyedList Key (List (Keyed a))


type Keyed a
    = Keyed Key a


type Key
    = Key Int


empty : KeyedList a
empty =
    KeyedList (Key 0) []


cons : a -> KeyedList a -> KeyedList a
cons item (KeyedList key items) =
    Keyed key item
        :: items
        |> KeyedList (next key)


push : a -> KeyedList a -> KeyedList a
push item (KeyedList key items) =
    Keyed key item
        |> List.singleton
        |> (++) items
        |> KeyedList (next key)


next : Key -> Key
next (Key key) =
    Key (key + 1)


fromList : List a -> KeyedList a
fromList items =
    let
        fromListHelper index item =
            Keyed (Key index) item
    in
        List.indexedMap fromListHelper items
            |> KeyedList (Key <| List.length items)


toList : KeyedList a -> List a
toList items =
    let
        toListHelper _ item =
            item
    in
        keyedMap toListHelper items


keyedMap : (Key -> a -> b) -> KeyedList a -> List b
keyedMap fn (KeyedList _ items) =
    let
        keyedMapHelper (Keyed key item) =
            fn key item
    in
        List.map keyedMapHelper items


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


remove : Key -> KeyedList a -> KeyedList a
remove key (KeyedList nextKey items) =
    let
        removeHelper (Keyed k _) =
            k /= key
    in
        List.filter removeHelper items
            |> KeyedList nextKey


isEmpty : KeyedList a -> Bool
isEmpty (KeyedList _ items) =
    List.isEmpty items


length : KeyedList a -> Int
length (KeyedList _ items) =
    List.length items


map : (a -> b) -> KeyedList a -> KeyedList b
map fn (KeyedList nextKey items) =
    let
        mapHelper (Keyed key item) =
            Keyed key <| fn item
    in
        List.map mapHelper items
            |> KeyedList nextKey


filter : (a -> Bool) -> KeyedList a -> KeyedList a
filter condition (KeyedList nextKey items) =
    let
        filterHelper (Keyed _ item) =
            condition item
    in
        List.filter filterHelper items
            |> KeyedList nextKey
