module ListDict where

import List exposing ((::), any)

------------------------------------------------------------------------
-- List-based dicitonary implementation that doesn't rely on comparables

type alias Dict a b = List (a,b)

empty : Dict a b
empty = []

member : a -> Dict a b -> Bool
member key values = any ((==)key << fst) values

insert : a -> b -> Dict a b -> Dict a b
insert key value values =
  case values of
    []      -> [(key,value)]
    (x::xs) -> if   (fst x) == key
               then ((key, value)::xs)
               else x :: insert key value xs

remove : a -> Dict a b -> Dict a b
remove key values =
  case values of 
    []      -> []
    (x::xs) -> if (fst x) == key
               then xs
               else x :: remove key xs

get : a -> Dict a b -> Maybe b
get key values =
  case values of
  []      -> Nothing
  (x::xs) -> if (fst x) == key
             then Just <| snd x
             else get key xs

