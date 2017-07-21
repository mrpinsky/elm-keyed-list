module TrackedList exposing (..)

type TrackedList a
    = TrackedList (List (TrackedItem a)) Int


type TrackedItem a
    = Tracker Int a


track : List a -> TrackedList a
track list =
    TrackedList List.indexedMap Tracker list