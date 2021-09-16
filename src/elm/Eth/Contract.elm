module Eth.Contract exposing (contractList)

import CompoundComponents.Eth.Ethereum exposing (ContractAddress(..), getContractAddressString)
import Dict
import Eth.Config exposing (Config)


contractList : Maybe Config -> List ( String, String )
contractList maybeNetworkConfig =
    let
        contractsRaw =
            case maybeNetworkConfig of
                Just config ->
                    let
                        namedContract : String -> ContractAddress -> ( String, String )
                        namedContract name entry =
                            ( name, getContractAddressString entry )

                        expendableContracts =
                            [ Maybe.map (namedContract "Timelock") config.maybeTimelock
                            , Maybe.map (namedContract "CErc20Delegate") config.maybeCErc20Delegate
                            , Maybe.map (namedContract "CDaiDelegate") config.maybeCDaiDelegate
                            , Maybe.map (namedContract "StdComptrollerG3") config.maybeComptrollerG3
                            , Maybe.map (namedContract "Starport") config.maybeStarport
                            , Maybe.map
                                (\( contractAddress, isBravo ) ->
                                    if isBravo then
                                        namedContract "GovernorBravo" contractAddress

                                    else
                                        namedContract "GovernorAlpha" contractAddress
                                )
                                config.maybeGovernor
                            ]
                                |> List.filterMap identity
                    in
                    [ ( "Comptroller", getContractAddressString config.comptroller )
                    , ( "PriceOracle", getContractAddressString config.priceOracle )
                    ]
                        ++ (config.cTokens
                                |> Dict.toList
                                |> List.map
                                    (\( cTokenSymbol, cTokenConfig ) ->
                                        let
                                            addressString =
                                                getContractAddressString cTokenConfig.address

                                            underlyingAddress =
                                                getContractAddressString cTokenConfig.underlying.address
                                        in
                                        [ ( cTokenSymbol, addressString ), ( cTokenConfig.underlying.symbol, underlyingAddress ) ]
                                    )
                                |> List.concat
                           )
                        ++ expendableContracts

                Nothing ->
                    []
    in
    contractsRaw
        |> List.sortBy Tuple.first
