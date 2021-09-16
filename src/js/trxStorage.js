import createStorage from './storage';

function trxStorage(storageKey) {
  const storage = createStorage(storageKey);

  function put(trxHash, network, timestamp, contractAddress, asset, customer, fun, args, status, error, expectedNonce) {
    const transactions = storage.get({});

    transactions[trxHash] = {
      trxHash,
      network,
      timestamp,
      contractAddress,
      asset,
      customer,
      fun,
      args,
      status,
      error,
      expectedNonce
    }

    storage.set(transactions);
  }

  function update(trxHash, status, error) {
    const transactions = storage.get({});

    const currentTrx = transactions[trxHash];

    if (currentTrx) {
      transactions[trxHash] = Object.assign(
        {},
        currentTrx,
        {
          status: status,
          error: error
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

export default trxStorage;