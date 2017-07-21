module KeyedList
    exposing
        ( KeyedList
        , Key
        , isEmpty
        , length
        , push
        , remove
        , fromList
        , tracker
        , value
        )

{-| A library for lists of things you want to track. Kind of like Html.Keyed but for your data model.

# Basics
@docs empty, isEmpty, length, push, remove

# Converting
@ fromList, toList

#

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


fromList : List a -> KeyedList a
fromList list =
    List.indexedMap (flip tag) list
        |> KeyedList (Key <| List.length list)


toList : KeyedList a -> List a
toList (KeyedList _ kList) =
    List.map value kList


isEmpty : KeyedList a -> Bool
isEmpty (KeyedList _ list) =
    List.isEmpty list


length : KeyedList a -> Int
length (KeyedList _ list) =
    List.length list


push : a -> KeyedList a -> KeyedList a
push item (KeyedList key items) =
    Keyed key item
        :: items
        |> KeyedList (next key)


update : Key -> (a -> a) -> KeyedList a -> KeyedList a
update key fn (KeyedList nextKey kList) =
    let
        updateHelper (Keyed k val) =
            if k == key then
                Keyed k (fn val)
            else
                Keyed k val
    in
        List.map updateHelper kList
            |> KeyedList nextKey


remove : Key -> KeyedList a -> KeyedList a
remove key (KeyedList _ list) =
    let
        removeHelper item =
            tracker item /= key
    in
        List.filter removeHelper list
            |> KeyedList key


tracker : Keyed a -> Key
tracker (Keyed key _) =
    key


value : Keyed a -> a
value (Keyed _ val) =
    val


tag : a -> Int -> Keyed a
tag val key =
    Keyed (Key key) val


next : Key -> Key
next (Key key) =
    Key (key + 1)



-- I feel like anything related to rekeying is a bad idea, so I'm going to leave it out for now.
-- append : KeyedList a -> KeyedList a -> KeyedList a
-- append (KeyedList (Key key) firstItems) (KeyedList _ secondItems) =
--     rekeyFrom (Key key) secondItems
--         |> List.append firstItems
--         |> KeyedList (Key <| key + List.length secondItems)
-- rekeyFrom : Key -> List ( Key, a ) -> List ( Key, a )
-- rekeyFrom (Key start) list =
--     let
--         rekey shift (Keyed _ value ) =
--             Keyed (Key <| start + shift) value
--     in
--         List.indexedMap rekey list
