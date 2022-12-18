module AWS.KVEncode exposing
    ( KVPairs, KVField
    , encode
    , int, float, string, bool
    , list, dict
    , field, optional, object
    )

{-| KVEncode provides encoders to turn things into list of `(String, String)`
which can be used to build headers or query parameters. The encoding is one that
is accepted by many AWS services.

When encoding records (or objects) the following scheme is used:

A field with a simple value is encoded like this:

    ( "Field", "value" )

A field with a list of simple values is encoded like this:

    [ ( "Field.member.1", "value1" )
    , ( "Field.member.2", "value2" )
    , ...
    ]

A field with a single object value is encoded like this:

    [ ( "Field.InnerField1", "innerValue1" )
    , ( "Field.InnerField2", "innerValue2" )
    , ...
    ]

A field with a list of object values is encoded like this:

    [ ( "Field.member.1.InnerField1", "innerValue1" )
    , ( "Field.member.1.InnerField2", "innerValue2" )
    , ...
    ]


# Sets of KV String tuples and their encoder.

@docs KVPairs, KVField
@docs encode


# Encoders for simple types and collections.

@docs int, float, string, bool
@docs list, dict


# Encoders for objects with optional fields.

@docs field, optional, object

-}

import Dict exposing (Dict)


{-| Describes pairs of `(String, String)` tuples.

Note that this data structure also allows values on their own with no keys for
the convenience of keeping this API in a similar shape to JSON encoders.

-}
type KVPairs
    = Val String
    | ListKVPairs (List KVPairs)
    | Object (List ( String, KVPairs ))


{-| A field of a record to be encoded in `(String, String)` form.
-}
type KVField
    = Pair String KVPairs
    | Skip


{-| Encodes a String without a key.
-}
string : String -> KVPairs
string val =
    Val val


{-| Encodes an Int as a String without a key.
-}
int : Int -> KVPairs
int val =
    String.fromInt val |> Val


{-| Encodes a Float as a String without a key.
-}
float : Float -> KVPairs
float val =
    String.fromFloat val |> Val


{-| Encodes an Bool as a String ("true" or "false") without a key.
-}
bool : Bool -> KVPairs
bool val =
    if val then
        "true" |> Val

    else
        "false" |> Val


{-| Encodes a list of items as `KVPairs`.
-}
list : (a -> KVPairs) -> List a -> KVPairs
list enc vals =
    List.map enc vals |> ListKVPairs


{-| Combines a Dict with an encoder for its values into a set of `KVPairs`.
-}
dict : (a -> KVPairs) -> Dict String a -> KVPairs
dict enc vals =
    Dict.foldr
        (\k v accum -> ( k, enc v ) :: accum)
        []
        vals
        |> Object


{-| Encodes a pair of `(String, a)` into `KVPairs`.
-}
field : (a -> KVPairs) -> ( String, a ) -> KVField
field enc ( name, val ) =
    Pair name (enc val)


{-| Encodes a pair of `(String, Maybe a)` into `KVPairs`.

Values that are `Nothing` are not encoded in the output, they are skipped over.

-}
optional : (a -> KVPairs) -> ( String, Maybe a ) -> KVField
optional enc ( name, maybeVal ) =
    case maybeVal of
        Nothing ->
            Skip

        Just val ->
            Pair name (enc val)


{-| Encodes a list of fields as an object.
-}
object : List KVField -> KVPairs
object fields =
    List.foldr
        (\fld accum ->
            case fld of
                Pair name val ->
                    ( name, val ) :: accum

                Skip ->
                    accum
        )
        []
        fields
        |> Object


{-| Encodes a set of KV pairs into `List (String, String)` form.

The keys in the output may be compound to describe a path through objects and
arrays into some deeper data structure. The possible formats are described in
the module level documentation above.

Note that only `KVPairs` that have keys are encoded by this. Simple values or
lists do not have keys and will result in `[]` being returned. This only produces
a non-empty result if the top-level `KVPair` passed in, describes an object.

-}
encode : KVPairs -> List ( String, String )
encode kvp =
    let
        encodeInner : KVPairs -> String -> List ( String, String ) -> List ( String, String )
        encodeInner innerKvp fldName accum =
            case innerKvp of
                Val val ->
                    ( fldName, val ) :: accum

                ListKVPairs vals ->
                    List.foldr
                        (\val ( idx, innerAccum ) ->
                            ( idx + 1
                            , encodeInner val (fldName ++ ".member." ++ String.fromInt idx) innerAccum
                            )
                        )
                        ( 1, accum )
                        vals
                        |> Tuple.second

                Object flds ->
                    List.foldr
                        (\( innerFldName, val ) innerAccum -> encodeInner val (fldName ++ "." ++ innerFldName) innerAccum)
                        accum
                        flds
    in
    case kvp of
        Val _ ->
            []

        ListKVPairs _ ->
            []

        Object flds ->
            List.foldr
                (\( name, val ) accum -> encodeInner val name accum)
                []
                flds



-- Example to test compilation against.


type alias Example =
    { field : List Inner
    , listField : List String
    , inner : Inner
    }


type alias Inner =
    { a : String
    , b : String
    }


encoder : Example -> KVPairs
encoder val =
    [ ( "field", val.field ) |> field (list innerEncoder)
    , ( "listField", val.listField ) |> field (list string)
    , ( "inner", val.inner ) |> field innerEncoder
    ]
        |> object


innerEncoder : Inner -> KVPairs
innerEncoder val =
    [ ( "a", val.a ) |> field string
    , ( "b", val.a ) |> field string
    ]
        |> object
