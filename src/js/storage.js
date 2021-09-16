
const USE_STORAGE = !!window.localStorage;

function storage(storageKey) {
  function getStorage(defaultValue) {
    const stored = localStorage.getItem(storageKey);

    if (stored) {
      try {
        return JSON.parse(stored);
      } catch (err) {
        // If we can't parse what we read from storage,
        // let's wipe it so the page doesn't break.
        clearStorage();
      }
    }

    return defaultValue;
  }

  function setStorage(item) {
    localStorage.setItem(storageKey, JSON.stringify(item));
  }

  function clearStorage() {
    localStorage.removeItem(storageKey);
  }

  function set(val) {
    if (USE_STORAGE) {
      setStorage(val);
    }
  }

  function get(defaultValue) {
    if (USE_STORAGE) {
      return getStorage(defaultValue);
    } else {
      return {};
    }
  }

  function clear() {
    if (USE_STORAGE) {
      clearStorage();
    }

    return true;
  }

  return {
    get,
    set,
    clear
  }
}

export default storage;