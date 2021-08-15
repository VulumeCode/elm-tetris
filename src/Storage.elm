port module Storage exposing (Storage, changes, decoder, highScore, storeHighScore)

import Json.Decode as Decode
import Json.Encode as Encode


type Storage
    = Storage { highScore : Int }


highScore : Storage -> Int
highScore (Storage storage) =
    storage.highScore


decoder : Decode.Decoder Storage
decoder =
    Decode.field "highScore" Decode.int
        |> Decode.map (\hs -> Storage { highScore = hs })


port store : Maybe Encode.Value -> Cmd msg

storeHighScore : Int -> Cmd msg
storeHighScore hs =
    let
        json =
            Encode.object [ ( "highScore", Encode.int hs ) ]
    in
    store (Just json)


clear : Cmd msg
clear =
    store Nothing


port onStoreChange : (Encode.Value -> msg) -> Sub msg


changes : (Maybe Storage -> msg) -> Sub msg
changes toMsg =
    onStoreChange (Decode.decodeValue decoder >> Result.toMaybe >> toMsg)
