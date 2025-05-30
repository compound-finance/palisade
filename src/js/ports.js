import { Sleuth } from '../../node_modules/@compound-finance/sleuth';
import { StaticJsonRpcProvider } from '../../node_modules/@ethersproject/providers';
import BN from '../../node_modules/bn.js';
import connectedWalletPorts from '../../node_modules/compound-components/src/js/sharedEth/connectedWalletPorts';
import EthUtils from '../../node_modules/web3-utils';
import FaucetToken from './json/contracts/FaucetToken.json';
import EIP20Interface from './json/contracts/EIP20Interface.json';
import { parseWeiStr, toScaledDecimal } from '../../node_modules/compound-components/src/js/sharedJs/math';
import trxStorage from './trxStorage';
import bnTxStorage from './bnTxStorage';
import storage from './storage';
import { requestForeground } from './helpers';
import {
  debug,
  langFromURL,
  shouldAutoConnect,
  supportFromEntries,
} from '../../node_modules/compound-components/src/js/sharedEth/utils';
import { subscribeToRepl } from './repl';
import {
  getAccounts,
  getBalance,
  getBlockNumber,
  getEvent,
  getLogs,
  getLedgerAddressAndBalance,
  getTransaction,
  getTransactionCount,
  getTransactionReceipt,
  makeEth,
  sign,
  withWeb3Eth,
  withTrxWeb3,
  withGasLimitFromPayload,
  wrapCall,
  wrapCallErr,
  wrapSend,
} from '../../node_modules/compound-components/src/js/sharedEth/eth';

import SleuthQuery from '../sleuth/out/SleuthLens.sol/SleuthLens.json';

const PROVIDER_TYPE_NONE = 0;
const PROVIDER_TYPE_LEDGER = 1;
const PROVIDER_TYPE_WALLET_LINK = 2;
const PROVIDER_TYPE_WEB3 = 3;
const PROVIDER_TYPE_SHOW_ACCOUNT = 3;

const ACCOUNT_CHECK_INTERVAL_MS = 2000;
const NETWORK_CHECK_INTERVAL_MS = 4000;
const NEW_BLOCK_CHECK_INTERVAL_MS = 60_000;
const SECONDS_PER_BLOCK = 12;
const BLOCKS_PER_DAY = new BN(7200); // 12 seconds per block
const EXP_DECIMALS = 18;
const CALCULATE_ACCOUNT_VALUES_DECIMALS = 36;
const EXP_SCALE_BN = new BN(10).pow(new BN(18)); // 1e18 used for BN.div
const defaultCallParams = { gas: 1.0e10 };

const transactionStorage = trxStorage('transactions');
const preferencesStorage = storage('preferences');

// We wait this amount of milliseconds before informing Elm of a new block
// we've heard about from Web3. This is to give Infura time to clear caches
// and reduce the likelihood of getting stale data.
const NEW_BLOCK_DELAY = 1500;

var currentSendGasPrice;

function reportError(app) {
  return (error) => {
    // TODO call window.on_error() with the stuff
    app.ports.giveError.send(error.toString());
  };
}

function getContractJsonByName(eth, contractName) {
  let targetContractAbi = eth.currentAbiMap[contractName];

  if (!targetContractAbi) {
    console.warn('Cannot find abi for contract: ', contractName);
    targetContractAbi = [];
  }

  return {
    contractName: contractName,
    abi: targetContractAbi,
  };
}

function getContractJsonByAddress(eth, contractAddress) {
  const contractName = eth.currentAddressToNameMap[contractAddress.toLowerCase()];
  return getContractJsonByName(eth, contractName);
}

async function getBlockTimestamps(blockNumbers, network) {
  if (blockNumbers.length === 0) {
    return {};
  } else {
    // TODO: Handle local network (e.g. by direct eth_getBlockByNumber calls)
    let timestampsResult = await fetch(
      `https://timestamp.compound.finance/${blockNumbers.join(',')}?network=${network}`
    );
    return await timestampsResult.json();
  }
}

async function handleReceipt(app, eth, trxHash, blockNumber, receipt, trxNonce) {
  // Ignore missing receipts or receipts that are beyond our knowledge of the
  // latest block (this is to provide consistency with the rest of the UI)
  if (!receipt || receipt.blockNumber > blockNumber) {
    return null;
  } else {
    // Comptroller, CToken & CEther all share the exact same event definition on failures
    // so we only need one of the to decode a failure in receipt logs.
    const Comptroller = getContractJsonByName(eth, 'Comptroller');
    const CToken = getContractJsonByAddress(eth, receipt.to);
    const nonOracleFailureEvent = getEvent(eth, Comptroller, 'Failure');

    const status = receipt.status === true ? 1 : 0;

    const failures = receipt.logs
      .map((log) => {
        if (nonOracleFailureEvent && nonOracleFailureEvent.matches(log)) {
          return nonOracleFailureEvent.decode(log);
        }
      })
      .filter((log) => !!log);

    var error = null;

    if (failures[0]) {
      // TODO: failure.info
      // TODO: failure.detail
      error = failures[0].error.toString(); // TODO: This should be a number
    }

    app.ports.giveUpdateTrxPort.send({
      trxHash: trxHash,
      status: status,
      error: error,
      trxNonce: trxNonce,
    });
  }
}

function subscribeToCTokenPorts(app, eth) {
  // port askLiquidatePort : { cTokenAddress : String, customerAddress : String, borrowerAddress : String, borrowedAssetAmountWeiStr : String, borrowedAssetDecimals : Int, desiredAssetAddress : String, desiredAssetDecimals : Int, isCEther : Bool  } -> Cmd msg
  app.ports.askLiquidatePort.subscribe(
    ({
      cTokenAddress,
      customerAddress,
      borrowerAddress,
      borrowedAssetAmountWeiStr,
      borrowedAssetDecimals,
      desiredAssetAddress,
      desiredAssetDecimals,
      isCEther,
    }) => {
      const CEther = getContractJsonByName(eth, 'cETH');
      const CToken = getContractJsonByAddress(eth, cTokenAddress);
      const closeAmountWei = parseWeiStr(borrowedAssetAmountWeiStr);

      if (isCEther) {
        wrapSend(
          app,
          eth,
          CEther,
          cTokenAddress,
          'liquidateBorrow',
          [borrowerAddress, desiredAssetAddress],
          cTokenAddress,
          customerAddress,
          currentSendGasPrice,
          {
            value: closeAmountWei,
            displayArgs: [cTokenAddress, closeAmountWei, desiredAssetAddress],
          }
        )
          .then((trxHash) => {
            app.ports.giveLiquidatePort.send({
              borrowerAddress: borrowerAddress,
              borrowedAssetAddress: cTokenAddress,
              borrowedAmount: toScaledDecimal(closeAmountWei, borrowedAssetDecimals),
              desiredCollateralAddress: desiredAssetAddress,
            });
          })
          .catch(reportError(app));
      } else {
        wrapSend(
          app,
          eth,
          CToken,
          cTokenAddress,
          'liquidateBorrow',
          [borrowerAddress, closeAmountWei, desiredAssetAddress],
          cTokenAddress,
          customerAddress,
          currentSendGasPrice,
          {
            displayArgs: [cTokenAddress, closeAmountWei, desiredAssetAddress],
          }
        )
          .then((trxHash) => {
            app.ports.giveLiquidatePort.send({
              borrowerAddress: borrowerAddress,
              borrowedAssetAddress: cTokenAddress,
              borrowedAmount: toScaledDecimal(closeAmountWei, borrowedAssetDecimals),
              desiredCollateralAddress: desiredAssetAddress,
            });
          })
          .catch(reportError(app));
      }
    }
  );

  function handleNonAccountQueryResults(app, cTokens, slethResponse) {
    const cTokenMetadataList = slethResponse.cTokens.map(
      (
        {
          cToken: cTokenAddress,
          exchangeRateCurrent: exchangeRateResult,
          supplyRatePerBlock: supplyRateResult,
          borrowRatePerBlock: borrowRateResult,
          reserveFactorMantissa: reserveFactorResult,
          totalBorrows: totalBorrowsResult,
          totalReserves: totalReservesResult,
          totalSupply: totalSupplyResult,
          totalCash: totalCashResult,
          isListed: isListedResult,
          collateralFactorMantissa: collateralFactorMantissaResult,
          underlyingAssetAddress: underlyingAssetAddress,
          cTokenDecimals: cTokenDecimals,
          underlyingDecimals: underlyingDecimals,
          compSupplySpeed: compSupplySpeedResult,
          compBorrowSpeed: compBorrowSpeedResult,
          borrowCap: borrowCapResult,
          mintGuardianPaused: mintGuardianPausedResult,
          underlyingPrice: underlyingPriceResult,
        },
        index
      ) => {
        const totalCash = toScaledDecimal(totalCashResult, underlyingDecimals);

        //Calculate oneCTokenInUnderlying
        const exchangeRateCurrent = exchangeRateResult;
        const mantissa = 18 + parseInt(underlyingDecimals) - cTokenDecimals;
        const oneCTokenInUnderlying = exchangeRateCurrent / Math.pow(10, mantissa);
        const totalSupplyScaled = totalSupplyResult / Math.pow(10, cTokenDecimals);

        // APY daily compounding formula : ( 1 + 5760 * supplyRatePerBlock / 1e18 )^365 - 1
        // BN.js only handles ints so we will need to return
        // 5760 * supplyRatePerBlock / 1e18
        // from the port and have the Elm side do the fancier math with Decimal.
        return {
          cTokenAddress: cTokenAddress,
          exchangeRate: toScaledDecimal(exchangeRateResult, EXP_DECIMALS),
          supplyRatePerDay: toScaledDecimal(supplyRateResult * BLOCKS_PER_DAY, EXP_DECIMALS),
          borrowRatePerDay: toScaledDecimal(borrowRateResult * BLOCKS_PER_DAY, EXP_DECIMALS),
          collateralFactor: toScaledDecimal(collateralFactorMantissaResult, EXP_DECIMALS),
          reserveFactor: toScaledDecimal(reserveFactorResult, EXP_DECIMALS),
          totalBorrows: toScaledDecimal(totalBorrowsResult, underlyingDecimals),
          totalReserves: toScaledDecimal(totalReservesResult, underlyingDecimals),
          totalSupply: toScaledDecimal(totalSupplyResult, cTokenDecimals),
          totalSupplyUnderlying: toScaledDecimal(totalSupplyScaled * oneCTokenInUnderlying, 0),
          totalUnderlyingCash: totalCash,
          compSupplySpeedPerBlock: toScaledDecimal(compSupplySpeedResult, EXP_DECIMALS),
          compSupplySpeedPerDay: toScaledDecimal(compSupplySpeedResult * BLOCKS_PER_DAY, EXP_DECIMALS),
          compBorrowSpeedPerBlock: toScaledDecimal(compBorrowSpeedResult, EXP_DECIMALS),
          compBorrowSpeedPerDay: toScaledDecimal(compBorrowSpeedResult * BLOCKS_PER_DAY, EXP_DECIMALS),
          borrowCap: toScaledDecimal(borrowCapResult, underlyingDecimals),
          mintGuardianPaused: mintGuardianPausedResult,
          underlyingPrice: toScaledDecimal(underlyingPriceResult, EXP_DECIMALS),
        };
      }
    );

    app.ports.giveCTokenMetadataPort.send(cTokenMetadataList);

    let allPricesList = cTokenMetadataList.map(({ cTokenAddress, underlyingPrice }) => {
      let underlyingAssetAddress = cTokens[cTokenAddress.toLowerCase()].underlyingAssetAddress;

      return {
        underlyingAssetAddress: underlyingAssetAddress,
        value: underlyingPrice,
      };
    });

    app.ports.giveOraclePricesAllPort.send(allPricesList);

    app.ports.giveComptrollerMetadataPort.send({
      closeFactor: toScaledDecimal(slethResponse.closeFactorMantissa, EXP_DECIMALS),
      liquidationIncentive: toScaledDecimal(slethResponse.liquidationIncentiveMantissa, EXP_DECIMALS),
    });
  }

  //port queryAllNoAccountPort : { blockNumber : Int, cTokens : List ( String, CTokenPortData ), comptroller : String } -> Cmd msg
  app.ports.queryAllNoAccountPort.subscribe(async ({ blockNumber, cTokens: cTokenEntries, comptroller }) => {
    let cTokens = supportFromEntries(cTokenEntries);

    const web3 = await withWeb3Eth(eth);

    const QUERY = Sleuth.querySol(SleuthQuery, { queryFunctionName: 'queryAllNoAccount' });

    const provider = new StaticJsonRpcProvider(web3.currentProvider.host);
    let sleuth = new Sleuth(provider);
    let response = await sleuth.fetch(QUERY, [Object.keys(cTokens)]);

    handleNonAccountQueryResults(app, cTokens, response);
  });

  // port queryAllWithAccountPort : { blockNumber : Int, customerAddress : String, cTokens : List ( String, CTokenPortData ), compAddress: String, capFactoryAddress: String } -> Cmd msg
  app.ports.queryAllWithAccountPort.subscribe(
    async ({ blockNumber, customerAddress, cTokens: cTokenEntries, compAddress, capFactoryAddress }) => {
      let cTokens = supportFromEntries(cTokenEntries);

      const web3 = await withWeb3Eth(eth);

      const QUERY = Sleuth.querySol(SleuthQuery, { queryFunctionName: 'queryAllWithAccount' });

      const provider = new StaticJsonRpcProvider(web3.currentProvider.host);
      let sleuth = new Sleuth(provider);

      Promise.all([
        getTransactionCount(eth, customerAddress),
        sleuth.fetch(QUERY, [Object.keys(cTokens), customerAddress, compAddress, capFactoryAddress]),
      ])
        .then(([accountTrxCount, response]) => {
          handleNonAccountQueryResults(app, cTokens, response);

          const cTokenBalancesList = response.cTokens.map(
            ({
              cToken: cTokenAddress,
              balanceOf: cTokenWalletBalanceResult,
              borrowBalanceCurrent: underlyingBorrowBalanceResult,
              balanceOfUnderlying: underlyingSupplyBalanceResult,
              tokenBalance: tokenBalanceResult,
              tokenAllowance: tokenAllowanceResult,
            }) => {
              let { underlyingAssetAddress, underlyingDecimals, cTokenDecimals, cTokenSymbol } =
                cTokens[cTokenAddress.toLowerCase()];
              const walletBalance = toScaledDecimal(cTokenWalletBalanceResult, cTokenDecimals);
              const borrowBalance = toScaledDecimal(underlyingBorrowBalanceResult, underlyingDecimals);
              const supplyBalance = toScaledDecimal(underlyingSupplyBalanceResult, underlyingDecimals);
              const tokenBalance = toScaledDecimal(tokenBalanceResult, underlyingDecimals);
              const tokenAllowance = toScaledDecimal(tokenAllowanceResult, underlyingDecimals);

              if (cTokenSymbol == 'cETH') {
                // Since we're on eth anyway
                app.ports.giveAccountBalancePort.send({
                  balance: tokenBalance,
                });
              }

              return {
                cTokenAddress: cTokenAddress,
                customerAddress: customerAddress,
                cTokenWalletBalance: walletBalance,
                underlyingAssetAddress: underlyingAssetAddress,
                underlyingBorrowBalance: borrowBalance,
                underlyingSupplyBalance: supplyBalance,
                underlyingTokenWalletBalance: tokenBalance,
                underlyingTokenAllowance: tokenAllowance,
              };
            }
          );

          app.ports.giveCTokenBalancesAllPort.send(cTokenBalancesList);

          app.ports.giveAccountLimitsPort.send({
            customerAddress: customerAddress,
            accountLiquidity: toScaledDecimal(response.liquidity, EXP_DECIMALS),
            accountShortfall: toScaledDecimal(response.shortfall, EXP_DECIMALS),
            assetsIn: response.marketsIn,
            trxCount: accountTrxCount,
          });

          app.ports.giveGovernanceDataPort.send({
            customerAddress: customerAddress,
            compTokenBalance: toScaledDecimal(response.compMetadata.balance, EXP_DECIMALS),
            currentVotesBalance: toScaledDecimal(response.compMetadata.votes, EXP_DECIMALS),
            delegateeAddress: response.compMetadata.delegate,
          });

          app.ports.giveCompAccruedPort.send({
            customerAddress: customerAddress,
            compAccrued: toScaledDecimal(response.compMetadata.allocated, EXP_DECIMALS),
          });

          app.ports.giveTokenAllowanceTokenPort.send({
            assetAddress: compAddress,
            contractAddress: capFactoryAddress,
            customerAddress: customerAddress,
            allowance: toScaledDecimal(response.capFactoryAllowance, EXP_DECIMALS),
          });
        })
        .catch(reportError(app));
    }
  );
}

function subscribeToNewBlocks(app, eth) {
  var blockTimer;
  var previousBlock;

  function newBlockCheckFunction() {
    requestForeground(() => {
      getBlockNumber(eth)
        .then((blockNumber) => {
          if (blockNumber && blockNumber !== previousBlock) {
            debug(`New Block: ${blockNumber}`);
            app.ports.giveNewBlockPort.send({ block: blockNumber });
            previousBlock = blockNumber;
          }
        })
        .catch(reportError(app))
        .finally(() => {
          blockTimer = setTimeout(newBlockCheckFunction, NEW_BLOCK_CHECK_INTERVAL_MS);
        });
    });
  }

  // port askNewBlockPort : {} -> Cmd msg
  app.ports.askNewBlockPort.subscribe(newBlockCheckFunction);

  // port askNewBlockAsyncPort : { blockNumber : Int } -> Cmd msg
  app.ports.askNewBlockAsyncPort.subscribe(({blockNumber}) => {
    requestForeground(() => {
      getBlockNumber(eth)
        .then((blockNumber) => {
          if (blockNumber && blockNumber !== previousBlock) {
            debug(`New Block: ${blockNumber}`);
            app.ports.giveNewBlockPort.send({ block: blockNumber });
            previousBlock = blockNumber;
          }
        })
        .catch(reportError(app))
    });
  });
}

function subscribeToCheckTrxStatus(app, eth) {
  // port checkTrxStatusPort : { blockNumber : Int, trxHash : String } -> Cmd msg
  app.ports.checkTrxStatusPort.subscribe(({ blockNumber, trxHash }) => {
    Promise.all([getTransaction(eth, trxHash), getTransactionReceipt(eth, trxHash)])
      .then(([transaction, receipt]) => handleReceipt(app, eth, trxHash, blockNumber, receipt, transaction.nonce))
      .catch(reportError(app));
  });
}

function subscribeToStoreTransaction(app, eth) {
  // port storeTransactionPort : { trxHash : String, networkId : Int, timestamp : Int, contractAddress : String, assetAddress : String, customerAddress : String, fun : String, args : List, status : Maybe Int, error : Maybe String, expectedNonce : Maybe Int } -> Cmd msg
  app.ports.storeTransactionPort.subscribe(
    ({ trxHash, networkId, timestamp, contractAddress, customerAddress, fun, args, status, error, expectedNonce }) => {
      transactionStorage.put(
        trxHash,
        networkId,
        timestamp,
        contractAddress,
        customerAddress,
        fun,
        args,
        status,
        error,
        expectedNonce
      );
    }
  );

  // port storeTransactionUpdatePort : { trxHash : String, status : Maybe Int, error : Maybe String } -> Cmd msg
  app.ports.storeTransactionUpdatePort.subscribe(({ trxHash, status, error }) => {
    transactionStorage.update(trxHash, status, error);
  });

  // port askStoredTransactionsPort : {} -> Cmd msg
  app.ports.askStoredTransactionsPort.subscribe(({}) => {
    const transactions = Object.values(transactionStorage.getAll());

    app.ports.giveStoredTransactionsPort.send(transactions);
  });

  // port askClearTransactionsPort : {} -> Cmd msg
  app.ports.askClearTransactionsPort.subscribe(({}) => {
    transactionStorage.clear();
  });
}

function subscribeToPreferences(app, eth) {
  const url = new URL(window.location);
  const lang = langFromURL(url, window.navigator.language);
  // port storePreferencesPort : { displayCurrency : String, userLanguage : String, supplyPaneOpen : Bool, borrowPaneOpen : Bool } -> Cmd msg
  app.ports.storePreferencesPort.subscribe((preferences) => {
    preferencesStorage.set(preferences);
  });

  // port askStoredPreferencesPort : {} -> Cmd msg
  app.ports.askStoredPreferencesPort.subscribe(() => {
    const preferences = preferencesStorage.get();
    app.ports.giveStoredPreferencesPort.send({ userLanguage: lang, ...preferences });
  });

  // port askClearPreferencesPort : {} -> Cmd msg
  app.ports.askClearPreferencesPort.subscribe(() => {
    preferencesStorage.set({});
  });
}

function subscribeToGasService(app) {
  // port setGasPricePort : { amountWeiStr : String } -> Cmd msg
  app.ports.setGasPricePort.subscribe(({ amountWeiStr }) => {
    const gasPriceWei = parseWeiStr(amountWeiStr);

    currentSendGasPrice = gasPriceWei;
  });
}

function subscribeToConsole(app) {
  app.ports.log.subscribe((msg) => {
    console.error(msg);
  });
}

const decodeParameters = (abi, fnABI, data) => {
  const regex = /(\w+)\(([\w,]*)\)/;
  const res = regex.exec(fnABI);
  if (!res) {
    return {
      functionName: '',
      functionArgs: [],
      functionCall: '',
    };
  }
  const [_, fnName, fnInputs] = res;
  const inputTypes = fnInputs.split(',');
  const parameters = abi.decodeParameters(inputTypes, data);

  const args =
    fnInputs.length > 0
      ? inputTypes.map((_, index) => {
          const parameter = parameters[index];
          return parameter ? parameter.toString() : '';
        })
      : [];

  return {
    functionName: fnName,
    functionArgs: args,
    functionCall: `${fnName}(${args.join(', ')})`,
  };
};

const isStale = (eta) => {
  const now = Date.now() / 1000;
  const gracePeriod = 1209600; // Timelock constant of 14 days
  return now > Number(eta) + gracePeriod;
};

function subscribeToAdminDashboard(app, eth) {
  // port adminDashboardGetQueuedTransactionsPort : { timelockAddress : String, blockNumber: Int } -> Cmd msg
  app.ports.adminDashboardGetQueuedTransactionsPort.subscribe(async ({ timelockAddress, initialBlockNumber }) => {
    const web3 = await withWeb3Eth(eth);
    const Timelock = getContractJsonByName(eth, 'Timelock');
    const queueEvent = getEvent(eth, Timelock, 'QueueTransaction');

    const pastQueuedTransactions = await web3.getPastLogs({
      fromBlock: initialBlockNumber,
      toBlock: 'latest',
      address: timelockAddress,
      topics: [queueEvent.signature],
    });

    const decodedEvents = pastQueuedTransactions.map((txLog, index) => {
      const decodedEvent = queueEvent.decode(txLog);
      const functionData = decodeParameters(web3.abi, decodedEvent.signature, decodedEvent.data);

      return {
        ...decodedEvent,
        transactionData: {
          ...functionData,
          ...txLog,
          timelockTrxNumber: index,
        },
      };
    });

    const calls = decodedEvents.map((event) => [Timelock, timelockAddress, 'queuedTransactions', [event.txHash]]);

    const txStillQueued = await wrapCall(app, eth, calls);

    const queuedTxs = decodedEvents.reduce((accum, event, index) => {
      if (txStillQueued[index] && !isStale(event.eta)) {
        return [...accum, event];
      }

      return accum;
    }, []);

    app.ports.giveQueuedTransactionsPort.send(queuedTxs);
  });

  // port adminDashboardEncodeParametersPort : { argTypes : List String, args : List String } -> Cmd msg
  app.ports.adminDashboardEncodeParametersPort.subscribe(async ({ argTypes, args }) => {
    const encodedParams = eth.trxEth.abi.encodeParameters(argTypes, args);

    app.ports.giveEncodedParametersPort.send(encodedParams);
  });

  // port adminDashboardSubmitProposalPort : { adminAddress: String, governorAddress : String, isBravo : Bool, targets : List String, values : List String, signatures : List String, calldatas : List String, description : String } -> Cmd msg
  app.ports.adminDashboardSubmitProposalPort.subscribe(
    async ({ adminAddress, governorAddress, isBravo, targets, values, signatures, calldatas, description }) => {
      const Governor = isBravo
        ? getContractJsonByName(eth, 'GovernorBravo')
        : getContractJsonByName(eth, 'GovernorAlpha');

      try {
        const trxHash = await wrapSend(
          app,
          eth,
          Governor,
          governorAddress,
          'propose',
          [targets, values, signatures, calldatas, description],
          governorAddress,
          adminAddress,
          currentSendGasPrice,
          {
            displayArgs: [targets, values, signatures, calldatas, description],
          }
        );
      } catch (e) {
        app.ports.giveError.send(e.toString());
      }
    }
  );

  // port adminDashboardSubmitCrowdProposalPort : { adminAddress: String, crowdFactoryAddress : String, targets : List String, values : List String, signatures : List String, calldatas : List String, description : String, compAddress : String, governorAddress : String } -> Cmd msg
  app.ports.adminDashboardSubmitCrowdProposalPort.subscribe(
    async ({
      adminAddress,
      crowdFactoryAddress,
      targets,
      values,
      signatures,
      calldatas,
      description,
      compAddress,
      governorAddress,
    }) => {
      const CrowdProposalFactory = getContractJsonByName(eth, 'CrowdProposalFactory');

      try {
        const trxHash = await wrapSend(
          app,
          eth,
          CrowdProposalFactory,
          crowdFactoryAddress,
          'createCrowdProposal',
          [targets, values, signatures, calldatas, description],
          crowdFactoryAddress,
          adminAddress,
          currentSendGasPrice,
          {
            displayArgs: [targets, values, signatures, calldatas, description],
          }
        );
      } catch (e) {
        app.ports.giveError.send(e.toString());
      }
    }
  );

  //TODO: We should start to think about removing these since these are only valid if we have no Governor....
  // port adminDashboardQueueTransactionPort : { adminAddress: String, timelockAddress : String, target : String, value : String, signature : String, data : String, delay : String } -> Cmd msg
  app.ports.adminDashboardQueueTransactionPort.subscribe(
    async ({ adminAddress, timelockAddress, target, value, signature, data, delay }) => {
      const Timelock = getContractJsonByName(eth, 'Timelock');
      const delayBN = new BN(delay);
      const now = new BN(Math.floor(Date.now() / 1000));

      // Pad the eta with some time so that if minimum delay is passed in it can account for mining time and still exceed minimum delay
      const paddingBN = new BN(90);
      const eta = delayBN.add(now).add(paddingBN).toString();

      try {
        const trxHash = await wrapSend(
          app,
          eth,
          Timelock,
          timelockAddress,
          'queueTransaction',
          [target, value, signature, data, eta],
          timelockAddress,
          adminAddress,
          currentSendGasPrice,
          {
            displayArgs: [target, value, signature, data, eta],
          }
        );
        const web3 = eth.trxEth;
        const receipt = await web3.getTransactionReceipt(trxHash);

        let decodedEvent;

        if (receipt && receipt.logs && receipt.logs.length > 0) {
          const txLog = receipt.logs[0];
          decodedEvent = queueEvent.decode(txLog);

          const functionData = decodeParameters(web3.abi, decodedEvent.signature, decodedEvent.data);

          decodedEvent = {
            ...decodedEvent,
            transactionData: {
              ...functionData,
              ...txLog,
              timelockTrxNumber: index,
            },
          };
        }

        app.ports.giveQueuedTransactionsPort.send([decodedEvent]);
      } catch (e) {
        app.ports.giveError.send(e.toString());
      }
    }
  );

  app.ports.adminDashboardExecuteTransactionPort.subscribe(
    async ({ adminAddress, timelockAddress, target, value, signature, data, eta }) => {
      const Timelock = getContractJsonByName(eth, 'Timelock');

      try {
        const trxHash = await wrapSend(
          app,
          eth,
          Timelock,
          timelockAddress,
          'executeTransaction',
          [target, value, signature, data, eta],
          timelockAddress,
          adminAddress,
          currentSendGasPrice,
          {
            displayArgs: [target, value, signature, data, eta],
          }
        );
      } catch (e) {
        app.ports.giveError.send(e.toString());
      }
    }
  );
}

function subscribeToGovernancePorts(app, eth) {
  const compQuorum = new BN('400000000000000000000000');

  const getTimestamps = async (proposalDataAll, proposalMeta, network) => {
    let proposalDataBlocks = proposalDataAll
      .map((data) => {
        return [data.startBlock, data.endBlock];
      })
      .flat();

    let proposalMetaBlocks = Object.values(proposalMeta)
      .map((meta) => {
        return [meta.createBlock, meta.canceledBlock, meta.executedBlock];
      })
      .flat();

    let blockNumbers = proposalDataBlocks.concat(proposalMetaBlocks).filter((x) => !!x);
    let blockNumbersUniq = Array.from(new Set(blockNumbers));
    let timestamps = getBlockTimestamps(blockNumbersUniq, network);
    return timestamps;
  };

  const statesForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    const states = [
      pendingStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
      activeStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
      defeatedStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
      succeededStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
      queuedStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
      canceledStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
      executedStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
      expiredStateForProposal(proposal, meta, timestamps, currentBlock, currentTime),
    ];
    return states.filter((state) => state != null);
  };

  const pendingStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    const startTime = timestamps[meta.createBlock];
    let endTime = timestamps[proposal.startBlock];

    if (endTime == null) {
      endTime = Math.floor(currentTime + (proposal.startBlock - currentBlock) * SECONDS_PER_BLOCK);
    }

    return {
      state: 'pending',
      start_time: startTime,
      end_time: endTime,
      trx_hash: meta.createTrxHash,
    };
  };

  const activeStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    const startTime = timestamps[proposal.startBlock];
    const isActive =
      (meta.canceledBlock == null || proposal.startBlock < meta.canceledBlock) &&
      startTime != null &&
      startTime < currentTime;

    if (isActive) {
      // This assumes a block time of 12 seconds, similar to the API.
      const endTime =
        timestamps[proposal.endBlock] || currentTime + (proposal.endBlock - currentBlock) * SECONDS_PER_BLOCK;
      return {
        state: 'active',
        start_time: startTime,
        end_time: endTime,
      };
    } else {
      return null;
    }
  };

  const defeatedStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    const endTime = timestamps[proposal.endBlock];
    const forVotesBN = new BN(proposal.forVotes);
    const againstVotesBN = new BN(proposal.againstVotes);
    const isDefeated =
      (meta.canceledBlock == null || proposal.endBlock < meta.canceledBlock) &&
      endTime &&
      endTime < currentTime &&
      (forVotesBN.lte(againstVotesBN) || compQuorum.gt(forVotesBN));

    if (isDefeated) {
      return {
        state: 'defeated',
        start_time: endTime,
      };
    } else {
      return null;
    }
  };

  const succeededStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    const endTime = timestamps[proposal.endBlock];
    const forVotesBN = new BN(proposal.forVotes);
    const againstVotesBN = new BN(proposal.againstVotes);
    const isCanceled =
      (meta.canceledBlock == null || proposal.endBlock < meta.canceledBlock) &&
      endTime &&
      endTime < currentTime &&
      forVotesBN.gt(againstVotesBN) &&
      forVotesBN.gt(compQuorum);

    if (isCanceled) {
      return {
        state: 'succeeded',
        start_time: endTime,
      };
    } else {
      return null;
    }
  };

  const queuedStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    const eta = Number(proposal.eta);
    if (eta > 0) {
      const timelockDelay = 172800;
      return {
        state: 'queued',
        start_time: eta - timelockDelay,
        end_time: eta,
      };
    } else {
      return null;
    }
  };

  const canceledStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    if (proposal.canceled) {
      return {
        state: 'canceled',
        start_time: timestamps[meta.canceledBlock],
        trx_hash: meta.canceledTrxHash,
      };
    } else {
      return null;
    }
  };

  const executedStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    if (proposal.executed) {
      if (timestamps[meta.executedBlock] && meta.executedTrxHash) {
        return {
          state: 'executed',
          start_time: timestamps[meta.executedBlock],
          trx_hash: meta.executedTrxHash,
        };
      } else {
        console.log("GOT INTO EXECUTE STATE WHEN WE SHOULDN'T HAVE");
        console.log('executed proposal: ', proposal);
        console.log('executed proposal Meta: ', meta);
        console.log('executed proposal Timestamps: ', timestamps);
        return null;
      }
    } else {
      return null;
    }
  };

  const expiredStateForProposal = (proposal, meta, timestamps, currentBlock, currentTime) => {
    const timelockGracePeriod = 1209600; // Two weeks in seconds
    const expiredTime = proposal.eta + timelockGracePeriod;
    const isExpired = !proposal.canceled && !proposal.executed && proposal.eta > 0 && expiredTime < currentTime;

    if (isExpired) {
      return {
        state: 'expired',
        start_time: expiredTime,
      };
    } else {
      return null;
    }
  };

  const getAllProposals = async (
    app,
    eth,
    web3,
    { governorAddress, isBravo, compoundLens, initialBlockNumber, currentBlockNumber, network }
  ) => {
    const Governor = isBravo
      ? getContractJsonByName(eth, 'GovernorBravo')
      : getContractJsonByName(eth, 'GovernorAlpha');

    const ProposalCreated = getEvent(eth, Governor, 'ProposalCreated');
    const ProposalCanceled = getEvent(eth, Governor, 'ProposalCanceled');
    const ProposalExecuted = getEvent(eth, Governor, 'ProposalExecuted');

    const proposalLogs = await getLogs(
      app,
      eth,
      governorAddress,
      [[ProposalCreated.signature, ProposalCanceled.signature, ProposalExecuted.signature]],
      initialBlockNumber
    );

    const parseTitleDescriptionFromBody = (description) => {
      const match = description.match(/\s*#\s+([^\r\n]+)/);
      if (match === null) {
        return { title: 'Untitled', description };
      }
      const parsedDescription = description.slice(match.index + match[0].length);
      return {
        title: match[1],
        description: parsedDescription.trim(),
      };
    };

    let proposalEventHandlers = {
      [ProposalCreated.signature]: {
        decode: ProposalCreated.decode,
        build: (proposal, { description, values }, { blockNumber, transactionHash }) => {
          const meta = parseTitleDescriptionFromBody(description);

          return {
            ...proposal,
            title: meta.title,
            description: meta.description,
            createBlock: blockNumber,
            createTrxHash: transactionHash,
            values: values,
          };
        },
      },
      [ProposalCanceled.signature]: {
        decode: ProposalCanceled.decode,
        build: (proposal, event, log) => {
          return {
            ...proposal,
            canceledBlock: log.blockNumber,
            canceledTrxHash: log.transactionHash,
          };
        },
      },
      [ProposalExecuted.signature]: {
        decode: ProposalExecuted.decode,
        build: (proposal, event, log) => {
          return {
            ...proposal,
            executedBlock: log.blockNumber,
            executedTrxHash: log.transactionHash,
          };
        },
      },
    };

    let proposalMeta = proposalLogs.reduce((proposals, log) => {
      let handler = proposalEventHandlers[log.topics[0]];
      if (!handler) {
        throw new Error(`Unknown log event: ${log.topics[0]}`);
      }
      let event = handler.decode(log);
      let proposalId = event.id; // all events have such an id
      let proposal = proposals[proposalId] || {};

      return {
        ...proposals,
        [proposalId]: handler.build(proposal, event, log),
      };
    }, {});
    const CompoundLens = getContractJsonByName(eth, 'CompoundLens');
    const govProposalsFunction = isBravo ? 'getGovBravoProposals' : 'getGovProposals';
    const proposalIds = Object.keys(proposalMeta);
    const [proposalDataAll] = await wrapCall(app, eth, [
      [CompoundLens, compoundLens, govProposalsFunction, [governorAddress, proposalIds]],
    ]);

    let timestamps = await getTimestamps(proposalDataAll, proposalMeta, network);

    return await Promise.all(
      Object.entries(proposalMeta).map(async ([proposalId, meta]) => {
        let data = proposalDataAll.find(([k, ...rest]) => Number(k) === Number(proposalId));
        const states = statesForProposal(data, meta, timestamps, currentBlockNumber, Math.floor(Date.now() / 1000));
        const abstainVotes = isBravo ? data.abstainVotes : 0;
        let proposal = {
          proposalId: Number(data.proposalId),
          proposer: data.proposer,
          eta: Number(data.eta),
          targets: data.targets,
          signatures: data.signatures,
          calldatas: data.calldatas,
          startBlock: Number(data.startBlock),
          endBlock: Number(data.endBlock),
          forVotes: data.forVotes,
          againstVotes: data.againstVotes,
          abstainVotes: abstainVotes,
          canceled: data.canceled,
          executed: data.executed,
          values: meta.values,
          meta,
          states,
        };
        return proposal;
      })
    );
  };

  const getVoteReceiptsForVoterByProposalId = async (
    web3,
    { governorAddress, isBravo, compoundLens, voter, proposalIds }
  ) => {
    const CompoundLens = getContractJsonByName(eth, 'CompoundLens');
    const govReceiptsFunction = isBravo ? 'getGovBravoReceipts' : 'getGovReceipts';
    let [receipts] = await wrapCall(app, eth, [
      [CompoundLens, compoundLens, govReceiptsFunction, [governorAddress, voter, proposalIds]],
    ]);

    let votedReceipts = receipts.filter((r) => r.hasVoted);
    return supportFromEntries(
      await Promise.all(
        votedReceipts.map((receipt) => {
          const voteReceiptSuport = isBravo ? Number(receipt.support) : receipt.support ? 1 : 0;

          return [
            receipt.proposalId,
            {
              support: voteReceiptSuport,
              votes: receipt.votes,
              proposal_id: Number(receipt.proposalId),
            },
          ];
        })
      )
    );
  };

  const getPriorVotesforVoterByProposalId = async (
    web3,
    { compoundLens, governanceTokenAddress, voter, proposals, decimals, currentBlockNumber }
  ) => {
    const CompoundLens = getContractJsonByName(eth, 'CompoundLens');

    // Starting from proposal 43 on mainnet, proposals now have a voting delay before becoming
    // active so we need to query Lens with the proposals that are pending otherwise Lens will
    // return an expected error that voting has not begun.
    const [blockNumbers, pendingBlockNumbers] = proposals.reduce(
      ([startedBlocks, futureBlocks], proposal) => {
        if (proposal.startBlock <= currentBlockNumber) {
          startedBlocks.push(proposal.startBlock);
        } else {
          futureBlocks.push(proposal.startBlock);
        }
        return [startedBlocks, futureBlocks];
      },
      [[], []]
    );

    let [compVotes] = await wrapCall(app, eth, [
      [
        CompoundLens,
        compoundLens,
        'getCompVotes',
        [
          governanceTokenAddress,
          voter,
          Array.from(new Set(blockNumbers)), // uniq
        ],
      ],
    ]);

    const emptyVotes = pendingBlockNumbers.map((blockNumber) => {
      return {
        blockNumber: blockNumber,
        votes: 0,
      };
    });
    const allCompVotes = compVotes.concat(emptyVotes);

    return supportFromEntries(
      proposals.map((proposal) => {
        let compVote = allCompVotes.find(({ blockNumber }) => Number(proposal.startBlock) === Number(blockNumber));

        return [proposal.proposalId, toScaledDecimal(compVote.votes, decimals)];
      })
    );
  };

  // port getVoteDashboardDataPort : { governorAddress : String, isBravo : Bool, compoundLens : String, decimals : Int, initialBlockNumber : Int, currentBlockNumber : Int, voter : String, network : String } -> Cmd msg
  app.ports.getVoteDashboardDataPort.subscribe(
    async ({
      governorAddress,
      isBravo,
      compoundLens,
      governanceTokenAddress,
      decimals,
      initialBlockNumber,
      currentBlockNumber,
      voter,
      network,
    }) => {
      const web3 = await withWeb3Eth(eth);
      const proposals = await getAllProposals(app, eth, web3, {
        governorAddress,
        isBravo,
        compoundLens,
        initialBlockNumber,
        currentBlockNumber,
        network,
      });

      const proposalIds = proposals.map((proposal) => proposal.proposalId);
      const proposalVoteReceipts = await getVoteReceiptsForVoterByProposalId(web3, {
        governorAddress,
        isBravo,
        compoundLens,
        voter,
        proposalIds,
      });

      // Can probably filter these for just active proposals
      const priorVotes = await getPriorVotesforVoterByProposalId(web3, {
        compoundLens,
        governanceTokenAddress,
        voter,
        proposals,
        decimals,
        currentBlockNumber,
      });

      const getActions = (p) => {
        return p.targets.map((target, i) => {
          return {
            title: `${target}.${p.signatures[i]}`,
            value: p.values[i],
            target: target,
            signature: p.signatures[i],
            data: p.calldatas[i],
          };
        });
      };

      const giveProposals = proposals.map((p) => {
        return {
          id: p.proposalId,
          title: p.meta.title,
          description: p.meta.description,
          states: p.states,
          for_votes: p.forVotes,
          against_votes: p.againstVotes,
          abstain_votes: p.abstainVotes,
          actions: getActions(p),
          proposer: {
            display_name: null,
            image_url: null,
            account_url: null,
            address: p.proposer,
          },
        };
      });

      const returnData = {
        proposals: giveProposals,
        proposalVoteReceipts,
        priorVotes,
        network,
        voter,
      };

      app.ports.giveVoteDashboardDataPort.send(returnData);
    }
  );

  // port governanceVoteProposalPort : { adminAddress : String, governorAddress : String, isBravo : Bool, proposalId : String, supportValue : Int, reason : String } -> Cmd msg
  app.ports.governanceVoteProposalPort.subscribe(
    async ({ adminAddress, governorAddress, isBravo, proposalId, supportValue, reason }) => {
      const governorContractName = isBravo ? 'GovernorBravo' : 'GovernorAlpha';
      const governorCastVoteFunction = isBravo ? 'castVoteWithReason' : 'castVote';
      const Governor = getContractJsonByName(eth, governorContractName);

      let voteArgs = [];
      if (isBravo) {
        voteArgs = [proposalId, supportValue, reason];
      } else {
        const supportBool = supportValue == 1 ? true : false;
        voteArgs = [proposalId, supportBool];
      }

      try {
        const trxHash = await wrapSend(
          app,
          eth,
          Governor,
          governorAddress,
          governorCastVoteFunction,
          voteArgs,
          governorAddress,
          adminAddress,
          currentSendGasPrice,
          {
            displayArgs: [proposalId, supportValue],
          }
        );
      } catch (e) {
        app.ports.giveError.send(e.toString());
      }
    }
  );

  // port askDelegateToPort : { compTokenAddress : String, customerAddress : String, targetAddress : String } -> Cmd msg
  app.ports.askDelegateToPort.subscribe(({ compTokenAddress, customerAddress, targetAddress }) => {
    const CompToken = getContractJsonByName(eth, 'COMP');

    wrapSend(
      app,
      eth,
      CompToken,
      compTokenAddress,
      'delegate',
      [targetAddress],
      compTokenAddress,
      customerAddress,
      currentSendGasPrice,
      {
        displayArgs: [targetAddress],
      }
    ).catch(reportError(app));
  });

  // port askGovernorQueueProposalPort : { governorAddress : String, isBravo : Bool, customerAddress : String, proposalId : Int } -> Cmd msg
  app.ports.askGovernorQueueProposalPort.subscribe(({ governorAddress, isBravo, customerAddress, proposalId }) => {
    const governorContractName = isBravo ? 'GovernorBravo' : 'GovernorAlpha';
    const Governor = getContractJsonByName(eth, governorContractName);

    wrapSend(
      app,
      eth,
      Governor,
      governorAddress,
      'queue',
      [proposalId],
      governorAddress,
      customerAddress,
      currentSendGasPrice,
      {
        displayArgs: [proposalId],
      }
    ).catch(reportError(app));
  });

  // port askGovernorExecuteProposalPort : { governorAddress : String, isBravo : Bool, customerAddress : String, proposalId : Int } -> Cmd msg
  app.ports.askGovernorExecuteProposalPort.subscribe(({ governorAddress, isBravo, customerAddress, proposalId }) => {
    const governorContractName = isBravo ? 'GovernorBravo' : 'GovernorAlpha';
    const Governor = getContractJsonByName(eth, governorContractName);

    wrapSend(
      app,
      eth,
      Governor,
      governorAddress,
      'execute',
      [proposalId],
      governorAddress,
      customerAddress,
      currentSendGasPrice,
      {
        displayArgs: [proposalId],
      }
    ).catch(reportError(app));
  });
}

function handleTransactionNotification(app, eth, txModule, txId, txHash, status, blockNumber) {
  // port etherTransactionStatePort : (Json.Decode.Value -> msg) -> Sub msg
  app.ports.etherTransactionStatePort.send({
    txModule,
    txId,
    txHash: txHash,
    status: status,
    blockNumber: blockNumber,
  });
}

function subscribeToEtherPorts(app, eth) {
  // port etherSendTransactionPort : String -> Encode.Value -> Cmd msg
  app.ports.etherSendTransactionPort.subscribe(([txModule, txId, { from, to, data, value }]) => {
    const trxPayload = {
      from,
      to,
      data,
      value,
    };

    withTrxWeb3(
      eth,
      (web3Eth) => {
        withGasLimitFromPayload(web3Eth, trxPayload).then((estimatedGasLimit) => {
          let trxPayloadWithGasLimit = Object.assign(trxPayload, {
            gas: estimatedGasLimit,
          });
          web3Eth
            .sendTransaction(trxPayloadWithGasLimit)
            .on('transactionHash', (txHash) => {
              // If no blocknative, then let's try creating a non BN Transaction
              // so it can be watched for every new block.
              app.ports.giveNewNonBNTrxPort.send({
                txModule,
                txId,
                txHash: txHash,
              });

              // Finally let's trigger a reject of the BNTransaction so we don't try 2
              // of them.
              app.ports.etherTransactionRejectedPort.send({
                txModule,
                txId,
              });
            })
            .catch((e) => {
              // User denied transaction signature
              if (e.code === 4001) {
                // port etherTransactionRejectedPort : (Json.Decode.Value -> msg) -> Sub msg
                app.ports.etherTransactionRejectedPort.send({
                  txModule,
                  txId,
                });
              } else {
                console.log('Error sending transaction: ', e);
              }
            });
        });
      },
      () => {
        console.error('Cannot send transaction without transaction Web3');
      }
    );
  });

  // port etherWatchTransactionPort : ( String, Int, String ) -> Encode.Value -> Cmd msg
  app.ports.etherWatchTransactionPort.subscribe(([txModule, txId, txHash]) => {
    getTransactionReceipt(eth, txHash)
      .then((receipt) => {
        if (!receipt) {
          return null;
        } else {
          const status = receipt.status === true ? 'confirmed' : 'failed';

          // port etherTransactionStatePort : (Json.Decode.Value -> msg) -> Sub msg
          app.ports.etherTransactionStatePort.send({
            txModule,
            txId,
            txHash: txHash,
            status: status,
            blockNumber: null,
          });
        }
      })
      .catch(reportError(app));
  });
}

function subscribeToFlywheelPorts(app, eth) {
  // port askClaimCompPort : { comptrollerAddress : String, customerAddress : String, markets : List String } -> Cmd msg
  app.ports.askClaimCompPort.subscribe(({ comptrollerAddress, customerAddress, markets }) => {
    const Comptroller = getContractJsonByName(eth, 'Comptroller');

    wrapSend(
      app,
      eth,
      Comptroller,
      comptrollerAddress,
      'claimComp',
      [customerAddress, markets],
      comptrollerAddress,
      customerAddress,
      currentSendGasPrice,
      {
        displayArgs: [customerAddress],
      }
    ).catch(reportError(app));
  });
}

function subscribe(
  app,
  globEthereum,
  dataProviders,
  networkMap,
  networkAbiMap,
  defaultNetwork,
  configFiles,
  configAbiFiles,
  configNameToAddressMappings,
  walletConnectProjectId
) {
  const eth = makeEth(dataProviders, networkMap, networkAbiMap, configNameToAddressMappings, defaultNetwork);
  connectedWalletPorts.subscribe(app, eth, globEthereum, networkMap, defaultNetwork, walletConnectProjectId);

  subscribeToConsole(app);
  subscribeToCTokenPorts(app, eth);
  subscribeToNewBlocks(app, eth);
  subscribeToCheckTrxStatus(app, eth);
  subscribeToStoreTransaction(app, eth);
  subscribeToPreferences(app, eth);
  subscribeToGasService(app);
  subscribeToRepl(app, eth, configFiles, configAbiFiles, connectedWalletPorts.showAccount);
  subscribeToAdminDashboard(app, eth);
  subscribeToGovernancePorts(app, eth);
  subscribeToFlywheelPorts(app, eth);
  subscribeToEtherPorts(app, eth);
}

export default {
  subscribe,
};
