module Array.MultiDim
  ( MultiDim, repeat, initialize
  , inBounds, dims, get, set
  , toIndexedList, toFlatArray, map, indexedMap
  ) where

{-|

A class for multi-dimensional arrays, which stores the number of coordinates
at type-level.

Internally, the array is represented as a 1D array, so lookups
should be quite fast. However, we provide a safe, clean
interface for multi-dimensional lookups and updates.

To create an array, use a safe-list with the array dimensions:

    import List.SafeList exposing (cons, null)
    --Creates a 4x6x2 array of zeroes
    myArray =
      repeat (4 `cons` 6 `cons` 2 `cons` null ) 0

Accesses and updates use SafeList as well:

    import List.SafeList exposing (cons, null)
    updatedArray =
      case get (3 `cons` 1 `cons` 1 `cons` null ) myArray of
        Nothing -> myArray
        Just x ->
          set (3 `cons` 1 `cons` 1 `cons` null ) (2*x) myArray

If you ever give the wrong number of coordinates, you get a type error:

    --Won't compile
    updatedArray =
      case get (3 `cons` 1 `cons` 1 `cons` null ) myArray of
        Nothing -> myArray
        Just x ->
          set (3 `cons` 1 `cons` 1 `cons` null ) (2*x) myArray

We don't yet support any operations which change the size of the array,
such as push or append. For operations like `foldl` and `foldr`,
use the `toFlatArray` function, then the corresponding functions
on Array.Array.

#The Array type and creation functions
@docs MultiDim, repeat, initialize

#Get array info
@docs inBounds, dims

#Access and update
@docs get, set

#Convting to flat List or Array
@docs toIndexedList, toFlatArray

#Mapping functions
@docs map, indexedMap
-}

import Array exposing (Array)
import List.SafeList exposing (cons, null)
import Debug

{-| Opaque type for `n`-dimensional arrays containing type `a` -}
type MultiDim a n =
  MD {dims : List.SafeList.SafeList Int n, arr : Array a}

{- Given the dimensions of an array,
and some coordinates in that array,
find the corresponding position in the "flat"
internal 1D array.
-}
flattenCoords : List.SafeList.SafeList Int n -> List.SafeList.SafeList Int n -> Int
flattenCoords safeDims safeCoords =
  let
    dimensionOffsets =
      List.SafeList.scanl (\x y -> x * y) 1 safeDims
      |> List.SafeList.tail
    addOffset (offset, coord) resultSoFar =
      (offset * coord) + resultSoFar
    offsetCoordPairs =
      List.SafeList.map2 (,) dimensionOffsets safeCoords
  in
    List.foldl addOffset 0 <| List.SafeList.toList offsetCoordPairs


{- Given the dimensions of an array,
and an integer,
find the corresponding coordinates that flatten
to that integer
-}
expandCoords : List.SafeList.SafeList Int n -> Int -> List.SafeList.SafeList Int n
expandCoords dims flatCoord =
  let
    dimensionOffsets = List.SafeList.tail <| List.SafeList.scanl (\x y -> x * y) 1 dims
    --Do some modular arithmetic to find the right place
    mapFn (offset, lastRemainder ) =
      (lastRemainder // offset, lastRemainder % offset )
  in
    List.SafeList.mapl mapFn flatCoord dimensionOffsets


{-| Given array dimensions, and coordinates,
determine if the coordinates are in bounds for the dimensions. -}
inBounds : List.SafeList.SafeList Int n -> List.SafeList.SafeList Int n -> Bool
inBounds dims coords =
  List.SafeList.map2 (,) dims coords
  |> List.SafeList.all (\(dim, coord) -> coord >= 0 && coord < dim )


{-| Given a SafeList [n1, n2, ...] of array dimensions,
create an n1 x n2 x ... multi-dimensional array -}
repeat : List.SafeList.SafeList Int n -> a -> MultiDim a n
repeat dims elem =
  let
    arrSize = dims |> List.SafeList.toList |> List.product
  in
    MD
    { dims = dims
    , arr = Array.repeat arrSize elem}

{-| Given a SafeList of array dimensions,
and a function to generate an element for each coordinate,
create a new array with the given elements -}
initialize
  :  List.SafeList.SafeList Int n
  -> (List.SafeList.SafeList Int n -> a)
  -> MultiDim a n
initialize dims initFn =
  let
    arrSize = dims |> List.SafeList.toList |> List.product
    intFn = (expandCoords dims) >> initFn
  in
    MD
    { dims = dims
    , arr = Array.initialize arrSize intFn}


{-| Get the dimensions of an array -}
dims : MultiDim a n -> List.SafeList.SafeList Int n
dims (MD arr) = arr.dims


{-| Return Just the element if all given coordinates
are within the array's bounds. Otherwise, return nothing. -}
get : List.SafeList.SafeList Int n -> MultiDim a n -> Maybe a
get coords (MD mdArr) =
  if
    (inBounds mdArr.dims coords)
  then
    case Array.get (flattenCoords mdArr.dims coords) mdArr.arr of
      Nothing -> Debug.crash "In-bounds check doesn't agree with array dims"
      x -> x
  else
    Nothing


{-| Set the element at the given coordinates, if they all
are within the array's bounds. Otherwise, return the original array. -}
set : List.SafeList.SafeList Int n -> a -> MultiDim a n -> MultiDim a n
set coords elem (MD mdArr as origArr) =
  if
    (inBounds mdArr.dims coords)
  then
    let
      flatCoords = flattenCoords mdArr.dims coords
      newArr = Array.set flatCoords elem mdArr.arr
    in
      MD {mdArr | arr = newArr}
  else
    origArr


{-| Generate a list of the array's elements,
paired with their coordinates.
 -}
toIndexedList : MultiDim a n -> List (List.SafeList.SafeList Int n, a)
toIndexedList (MD mdArr) =
  Array.toIndexedList mdArr.arr
  |> List.map (\(flatIndex, elem) ->
                (expandCoords mdArr.dims flatIndex, elem ) )


{-| Convert this array to a 1-dimensional array
with elements in lexigraphical order -}
toFlatArray : MultiDim a n -> Array a
toFlatArray (MD mdArr) =
  mdArr.arr


{-| Apply a function on every element in an array -}
map : (a -> b) -> MultiDim a n -> MultiDim b n
map f (MD mdArr) =
  MD {mdArr | arr = Array.map f mdArr.arr}


{-| Apply a function on every element in an array,
with access to the coordinates of each element. -}
indexedMap
  : (List.SafeList.SafeList Int n -> a -> b)
  -> MultiDim a n
  -> MultiDim b n
indexedMap f (MD mdArr) =
  let
    flatFn =
      (expandCoords mdArr.dims) >> f
  in MD {mdArr | arr = Array.indexedMap flatFn mdArr.arr}


--Just make sure the examples compile
myArray =
  repeat (4 `cons` 6 `cons` 2 `cons` null ) 0

updatedArray =
  case get (3 `cons` 1 `cons` 1 `cons` null ) myArray of
    Nothing -> myArray
    Just x ->
      set (3 `cons` 1 `cons` 1 `cons` null ) (2*x) myArray

{- This shouldn't compile

updatedArray =
  case get (3 `cons` 1 `cons` null ) myArray of
    Nothing -> myArray
    Just x ->
      set (3 `cons` 1 `cons` null ) (2*x) myArray
-}
