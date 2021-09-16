module Ether.Contracts.Maximillion exposing (repayBehalf)

import BigInt exposing (BigInt)
import CompoundComponents.Console as Console
import CompoundComponents.Eth.Ethereum as Ethereum exposing (ContractAddress(..), CustomerAddress(..))
import CompoundComponents.Eth.Network exposing (Network(..))
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionState)
import CompoundComponents.Ether.FromEthereumUtils as FromEthereumUtils
import CompoundComponents.Ether.FunctionSpec as FunctionSpec
import CompoundComponents.Ether.Value as Value
import CompoundComponents.Ether.Web3 as EtherWeb3
import Eth.Config exposing (Config)



--function repayBehalf(address borrower) public payable


repayBehalf : Config -> Network -> CustomerAddress -> ContractAddress -> BigInt -> BNTransactionState -> ( BNTransactionState, Cmd msg )
repayBehalf config network customerAddress cTokenAddress repayUnderlyingAmountWei bnState =
    let
        ownerAddressResult =
            FromEthereumUtils.customerAddressToEtherAddress customerAddress

        dataResult =
            ownerAddressResult
                |> Result.andThen
                    (\ownerAddress ->
                        FunctionSpec.encodeCall
                            "repayBehalf"
                            [ Value.Address ownerAddress ]
                    )

        maximillionAddressResult =
            FromEthereumUtils.contractAddressToEtherAddress config.maximillion

        ( trx, cmd ) =
            case ( ownerAddressResult, maximillionAddressResult, dataResult ) of
                ( Ok fromAddress, Ok toAddress, Ok data ) ->
                    let
                        cTokenAddressString =
                            Ethereum.getContractAddressString cTokenAddress

                        amountString =
                            BigInt.toString repayUnderlyingAmountWei

                        bnTransaction =
                            BNTransaction.newTransaction network fromAddress toAddress "repayBehalf" [ cTokenAddressString, amountString ] bnState
                    in
                    ( Just bnTransaction
                    , EtherWeb3.sendTransactionWithValue repayUnderlyingAmountWei
                        (BNTransaction.getTxModule network customerAddress)
                        bnTransaction.txId
                        { from = fromAddress
                        , to = toAddress
                        , data = data
                        }
                    )

                _ ->
                    ( Nothing, Console.log "Could not encode data for Maximillion.repayBehalf" )
    in
    ( BNTransaction.appendTrx bnState trx, cmd )
