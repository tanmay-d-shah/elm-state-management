module Note exposing (Note, collectionDecoder, decoder)

import Json.Decode


type alias Note =
    { id : Int
    , title : String
    , content : String
    , categoryID : Int
    }


decoder : Json.Decode.Decoder Note
decoder =
    Json.Decode.map4 Note
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
        (Json.Decode.field "categoryID" Json.Decode.int)


collectionDecoder : Json.Decode.Decoder (List Note)
collectionDecoder =
    Json.Decode.map identity (Json.Decode.list decoder)
