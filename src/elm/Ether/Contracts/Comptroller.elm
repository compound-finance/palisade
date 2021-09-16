module Ether.Contracts.Comptroller exposing
    ( enterMarkets
    , exitMarket
    )

import CompoundComponents.Console as Console
import CompoundComponents.Eth.Ethereum as Ethereum exposing (ContractAddress(..), CustomerAddress(..))
import CompoundComponents.Eth.Network exposing (Network(..))
import CompoundComponents.Ether.Address
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionState)
import CompoundComponents.Ether.FromEthereumUtils as FromEthereumUtils
import CompoundComponents.Ether.FunctionSpec as FunctionSpec
import CompoundComponents.Ether.Spec as Spec
import CompoundComponents.Ether.Value as Value
import CompoundComponents.Ether.Web3 as EtherWeb3
import Eth.Config exposing (Config)



--function enterMarkets(address[] calldata cTokens)


enterMarkets : Config -> Network -> CustomerAddress -> ContractAddress -> List ContractAddress -> BNTransactionState -> ( BNTransactionState, Cmd msg )
enterMarkets config network customerAddress comptrollerAddress cTokenAddressList bnState =
    let
        cTokenListAsEtherAddresses =
            cTokenAddressList
                |> List.filterMap
                    (\cTokenAddress ->
                        case FromEthereumUtils.contractAddressToEtherAddress cTokenAddress of
                            Ok cTokenAsEtherAddress ->
                                Just cTokenAsEtherAddress

                            Err _ ->
                                Nothing
                    )

        dataResult =
            FunctionSpec.encodeCall
                "enterMarkets"
                [ Value.List Spec.Address (List.map (\address -> Value.Address address) cTokenListAsEtherAddresses) ]

        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        comptrollerAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress comptrollerAddress

        ( trx, cmd ) =
            case ( ownerAddressResult, comptrollerAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        addresses =
                            cTokenListAsEtherAddresses
                                |> List.map CompoundComponents.Ether.Address.toString

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "enterMarkets" addresses bnState
                    in
                    ( Just bnTransaction
                    , EtherWeb3.sendTransaction
                        (BNTransaction.getTxModule network customerAddress)
                        bnTransaction.txId
                        { from = fromAddress
                        , to = toAddress
                        , data = data
                        }
                    )

                _ ->
                    ( Nothing, Console.log "Could not encode data for Comptroller.enterMarkets" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )



--function exitMarket(address cToken)


exitMarket : Config -> Network -> CustomerAddress -> ContractAddress -> ContractAddress -> BNTransactionState -> ( BNTransactionState, Cmd msg )
exitMarket config network customerAddress comptrollerAddress cTokenAddress bnState =
    let
        cTokenAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress cTokenAddress

        dataResult =
            cTokenAddressResult
                |> Result.andThen
                    (\cTokenAsEtherAddress ->
                        FunctionSpec.encodeCall
                            "exitMarket"
                            [ Value.Address cTokenAsEtherAddress ]
                    )

        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        comptrollerAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress comptrollerAddress

        ( trx, cmd ) =
            case ( ownerAddressResult, comptrollerAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        addressString =
                            Ethereum.getContractAddressString cTokenAddress

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "exitMarket" [ addressString ] bnState
                    in
                    ( Just bnTransaction
                    , EtherWeb3.sendTransaction
                        (BNTransaction.getTxModule network customerAddress)
                        bnTransaction.txId
                        { from = fromAddress
                        , to = toAddress
                        , data = data
                        }
                    )

                _ ->
                    ( Nothing, Console.log "Could not encode data for Comptroller.exitMarket" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )
