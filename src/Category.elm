module Category exposing (Category, collectionDecoder, decoder)

import Json.Decode


type alias Category =
    { id : Int
    , name : String
    }


decoder : Json.Decode.Decoder Category
decoder =
    Json.Decode.map2 Category
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


collectionDecoder : Json.Decode.Decoder (List Category)
collectionDecoder =
    Json.Decode.map identity (Json.Decode.list decoder)
