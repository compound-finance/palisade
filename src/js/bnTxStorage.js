import createStorage from './storage';

function bnTxStorage(storageKey) {
  const storage = createStorage(storageKey);

  function put(txModuleId, timestamp, network, txId, txHash, txStatus, fromAddress, toAddress, func, args ) {
    const transactions = storage.get({});
    const transactionKey = txModuleId + "-" + txId;

    transactions[transactionKey] = {
      txModuleId,
      timestamp,
      network,
      txId,
      txHash,
      txStatus,
      fromAddress,
      toAddress,
      func,
      args
    }

    storage.set(transactions);
  }

  function update(txModuleId, txId, txHash, txStatus) {
    const transactions = storage.get({});
    const transactionKey = txModuleId + "-" + txId;

    const currentTrx = transactions[transactionKey];

    if (currentTrx) {
      transactions[transactionKey] = Object.assign(
        {},
        currentTrx,
        {
          txHash : txHash,
          txStatus: txStatus
        }
      )

      storage.set(transactions);
    }
  }

  function getAll() {
    const res = storage.get({});

    return res;
  }

  function clear() {
    storage.clear();

    return true;
  }

  return {
    put,
    update,
    getAll,
    clear
  }
}

export default bnTxStorage;