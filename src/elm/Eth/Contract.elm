module Eth.Contract exposing (ContractInfo, contractList)

import CompoundComponents.Eth.Ethereum exposing (ContractAddress(..), getContractAddressString)
import Dict
import Eth.Config exposing (Config)



type alias ContractInfo =
    { friendlyName : String
    , abiName : String
    , address : String
    }


contractList : Maybe Config -> List ContractInfo
contractList maybeNetworkConfig =
    let
        contractsRaw =
            case maybeNetworkConfig of
                Just config ->
                    let
                        namedContract : String -> ContractAddress -> ContractInfo
                        namedContract name entry =
                            { friendlyName = name
                            , abiName = name
                            , address = getContractAddressString entry 
                            }

                        friendlyNamedContract : String -> String -> ContractAddress -> ContractInfo
                        friendlyNamedContract friendlyName actualName entry =
                            { friendlyName = friendlyName
                            , abiName = actualName
                            , address = getContractAddressString entry 
                            }

                        expendableContracts =
                            [ Maybe.map (namedContract "Timelock") config.maybeTimelock
                            , Maybe.map (namedContract "CErc20Delegate") config.maybeCErc20Delegate
                            , Maybe.map (namedContract "CDaiDelegate") config.maybeCDaiDelegate
                            , Maybe.map (namedContract "StdComptrollerG3") config.maybeComptrollerG3
                            , Maybe.map (namedContract "Starport") config.maybeStarport
                            , Maybe.map (namedContract "cUSDCv3") config.maybeCUSDCv3
                            , Maybe.map (friendlyNamedContract "Configurator (v3)" "Configurator") config.maybeCUSDCv3Configurator
                            , Maybe.map (namedContract "Compoundv3Admin") config.maybeCUSDCv3Admin
                            , Maybe.map (namedContract "Compoundv3Rewards") config.maybeCUSDCv3Rewards
                            , Maybe.map (namedContract "Bulker") config.maybeCUSDCv3Bulker
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
                    [ { friendlyName = "Comptroller"
                        , abiName = "Comptroller"
                        , address = getContractAddressString config.comptroller 
                        }
                    , { friendlyName = "PriceOracle"
                        , abiName = "PriceOracle"
                        , address = getContractAddressString config.priceOracle 
                        }
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
                                        [ { friendlyName = cTokenSymbol
                                            , abiName = cTokenSymbol
                                            , address = addressString 
                                            }
                                        , { friendlyName = cTokenConfig.underlying.symbol
                                            , abiName = cTokenConfig.underlying.symbol
                                            , address = underlyingAddress 
                                            }

                                        ]
                                    )
                                |> List.concat
                           )
                        ++ expendableContracts

                Nothing ->
                    []
    in
    contractsRaw
        |> List.sortBy .friendlyName
