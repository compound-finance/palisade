import { at } from './utils';

// TODO: LocalStorage?
var localCache = {};
var localPromiseCache = {};

function getCacheKey(contractAddress, fun, args) {
  return `${contractAddress}:${fun}:${args.join(',')}`;
}

// Attempts to cache a Web3 response locally
// This should only be for values which are constant during a page load
function cache(eth, contract, contractAddress, fun, args) {
  const cacheKey = getCacheKey(contractAddress, fun, args);

  if (cacheKey in localCache) {
    return Promise.resolve(localCache[cacheKey]);
  } else if (cacheKey in localPromiseCache) {
    return localPromiseCache[cacheKey];
  } else {
    const promise = new Promise((resolve, reject) => {
      at(contract, contractAddress).methods[fun](...args).call().catch((error) => {
        reject(error);
      }).then((result) => {
        // This may have been set since we've last checked
        if (!(cacheKey in localCache)) {
          localCache[cacheKey] = result;
        }

        delete localPromiseCache[cacheKey];

        resolve(result);
      });
    });

    localPromiseCache[cacheKey] = promise;

    return promise;
  }
}

export {
  cache
}