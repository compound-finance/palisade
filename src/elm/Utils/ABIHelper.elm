module Utils.ABIHelper exposing (ABIInputOutputType, ABIValue)


type alias ABIInputOutputType =
    { name : String
    , type_ : String
    }


type alias ABIValue =
    { inputs : List ABIInputOutputType
    , name : String
    , outputs : List ABIInputOutputType
    , payable : Bool
    , stateMutability : String
    , type_ : String
    , signature : String
    }
