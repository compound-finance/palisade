module CompoundApi.Common.Models exposing (API_Error, PaginationSummary)


type alias API_Error =
    { error_code : Int
    , message : String
    }


type alias PaginationSummary =
    { page_number : Int
    , page_size : Int
    , total_entries : Int
    , total_pages : Int }