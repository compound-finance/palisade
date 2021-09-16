module Ether.Contracts.CToken exposing
    ( borrow
    , mint
    , redeem
    , redeemUnderlying
    , repayBorrow
    )

import BigInt exposing (BigInt)
import CompoundComponents.Console as Console
import CompoundComponents.Eth.Ethereum exposing (ContractAddress(..), CustomerAddress(..))
import CompoundComponents.Eth.Network exposing (Network(..))
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionState)
import CompoundComponents.Ether.FromEthereumUtils as FromEthereumUtils
import CompoundComponents.Ether.FunctionSpec as FunctionSpec
import CompoundComponents.Ether.Helpers as EtherHelpers
import CompoundComponents.Ether.Value as Value
import CompoundComponents.Ether.Web3 as EtherWeb3
import Eth.Config exposing (Config)



-- function borrow(uint borrowAmount) external returns (uint)


borrow : Config -> Network -> CustomerAddress -> ContractAddress -> BigInt -> BNTransactionState -> ( BNTransactionState, Cmd msg )
borrow config network customerAddress cTokenAddress underlyingAmountWei bnState =
    let
        dataResult =
            FunctionSpec.encodeCall
                "borrow"
                [ Value.UInt 256 underlyingAmountWei ]

        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        cTokenAssetAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress cTokenAddress

        ( trx, cmd ) =
            case ( ownerAddressResult, cTokenAssetAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        amountString =
                            underlyingAmountWei
                                |> BigInt.toString

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "borrow" [ amountString ] bnState
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
                    ( Nothing, Console.log "Could not encode data for CToken.borrow" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )



-- function mint(uint mintAmount) or function mint() external payable


mint : Config -> Network -> CustomerAddress -> ContractAddress -> BigInt -> BNTransactionState -> ( BNTransactionState, Cmd msg )
mint config network customerAddress cTokenAddress underlyingAmountWei bnState =
    let
        dataResult =
            FunctionSpec.encodeCall
                "mint"
                (if cTokenAddress == config.cEtherToken.address then
                    []

                 else
                    [ Value.UInt 256 underlyingAmountWei ]
                )

        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        cTokenAssetAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress cTokenAddress

        ( trx, cmd ) =
            case ( ownerAddressResult, cTokenAssetAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        amountString =
                            underlyingAmountWei
                                |> BigInt.toString

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "mint" [ amountString ] bnState

                        web3TransactionFunc =
                            if cTokenAddress == config.cEtherToken.address then
                                EtherWeb3.sendTransactionWithValue underlyingAmountWei

                            else
                                EtherWeb3.sendTransaction
                    in
                    ( Just bnTransaction
                    , web3TransactionFunc
                        (BNTransaction.getTxModule network customerAddress)
                        bnTransaction.txId
                        { from = fromAddress
                        , to = toAddress
                        , data = data
                        }
                    )

                _ ->
                    ( Nothing, Console.log "Could not encode data for CToken.mint" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )



-- function redeem(uint redeemTokens) external returns (uint)


redeem : Config -> Network -> CustomerAddress -> ContractAddress -> BigInt -> BNTransactionState -> ( BNTransactionState, Cmd msg )
redeem config network customerAddress cTokenAddress cTokenAmountWei bnState =
    let
        dataResult =
            FunctionSpec.encodeCall
                "redeem"
                [ Value.UInt 256 cTokenAmountWei ]

        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        cTokenAssetAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress cTokenAddress

        ( trx, cmd ) =
            case ( ownerAddressResult, cTokenAssetAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        amountString =
                            cTokenAmountWei
                                |> BigInt.toString

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "redeem" [ amountString ] bnState
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
                    ( Nothing, Console.log "Could not encode data for CToken.redeem" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )



--function redeemUnderlying(uint redeemAmount) external returns (uint)


redeemUnderlying : Config -> Network -> CustomerAddress -> ContractAddress -> BigInt -> BNTransactionState -> ( BNTransactionState, Cmd msg )
redeemUnderlying config network customerAddress cTokenAddress underlyingAmountWei bnState =
    let
        dataResult =
            FunctionSpec.encodeCall
                "redeemUnderlying"
                [ Value.UInt 256 underlyingAmountWei ]

        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        cTokenAssetAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress cTokenAddress

        ( trx, cmd ) =
            case ( ownerAddressResult, cTokenAssetAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        amountString =
                            underlyingAmountWei
                                |> BigInt.toString

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "redeemUnderlying" [ amountString ] bnState
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
                    ( Nothing, Console.log "Could not encode data for CToken.redeemUnderlying" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )



--function repayBorrow(uint repayAmount) external returns (uint)


repayBorrow : Config -> Network -> CustomerAddress -> ContractAddress -> BigInt -> BNTransactionState -> ( BNTransactionState, Cmd msg )
repayBorrow config network customerAddress cTokenAddress underlyingAmountWei bnState =
    let
        dataResult =
            FunctionSpec.encodeCall
                "repayBorrow"
                [ Value.UInt 256 underlyingAmountWei ]

        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        cTokenAssetAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress cTokenAddress

        ( trx, cmd ) =
            case ( ownerAddressResult, cTokenAssetAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        amountString =
                            if BigInt.compare underlyingAmountWei EtherHelpers.negativeOne == EQ then
                                "-1"

                            else
                                underlyingAmountWei
                                    |> BigInt.toString

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "repayBorrow" [ amountString ] bnState
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
                    ( Nothing, Console.log "Could not encode data for CToken.repayBorrow" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )
