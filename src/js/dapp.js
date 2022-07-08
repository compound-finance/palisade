import Eth from '../../node_modules/web3-eth';
import '../css-build/main.css';
import { Elm } from '../elm/Main.elm';
import registerServiceWorker from './registerServiceWorker';
import ports from './ports';
import storage from './storage';
import { providerType, langFromURL } from '../../node_modules/compound-components/src/js/sharedEth/utils';
import { MDCSwitch } from '@material/switch';

var oldErrorHandler = window.onerror || function () {};

window.onerror = function (message, source, lineno, colno, error) {
  oldErrorHandler(message, source, lineno, colno, error);
  return true;
};

window.addEventListener('load', function () {
  let globEthereum = null;
  if (window['ethereum']) {
    globEthereum = window['ethereum'];
  }

  const dataProviders = Object.entries(process.env.DATA_PROVIDERS).reduce((acc, [network, url]) => {
    return Object.assign(acc, { [network]: new Eth.providers.HttpProvider(url) });
  }, {});

  const configFiles = process.env.CONFIG_FILES;
  const configAbiFiles = process.env.CONFIG_ABI_FILES;

  console.log(configFiles);

  let configNameToAddressMappings = {};
  for (let [network, networkConfig] of Object.entries(configFiles)) {
    const contracts = networkConfig['Contracts'];

    //To prevent weirdness on lookups, let's lowercase the addresses
    let contractsWithLowerCaseAddresses = {};
    for (let [contractName, contractAddress] of Object.entries(contracts)) {
      contractsWithLowerCaseAddresses[contractName] = contractAddress.toLowerCase();
    }
    configNameToAddressMappings[network] = contractsWithLowerCaseAddresses;
  }

  console.log(configNameToAddressMappings);

  const url = new URL(window.location);

  let buildHref = (href) => href;
  let stripHref = (href) => href;
  let mountPath = window.BUILD_MOUNT_PATH || process.env.MOUNT_PATH || '';
  if (mountPath) {
    let mountRegex = new RegExp(`^/${mountPath}`);

    stripHref = (href) => {
      let url = new URL(href);
      url.pathname = url.pathname.replace(mountRegex, '');
      return url.href;
    };

    buildHref = (href) => {
      let url = new URL(href);
      url.pathname = mountPath + url.pathname;
      return url.href;
    };
  }

  const app = Elm.Main.init({
    node: document.getElementById('root') || document.getElementById('main'), // no pre-rendered // pre-rendered
    flags: {
      apiBaseUrlMap: process.env.API_BASE_URL_MAP,
      configurations: configFiles,
      configAbiFiles: configAbiFiles,
      language: langFromURL(url, window.navigator.language),
      path: stripHref(window.location.href),
      providerType: providerType(globEthereum),
      userAgent: navigator.userAgent,
      dataProviders: process.env.DATA_PROVIDERS,
    },
  });

  console.log(
    process.env.API_BASE_URL_MAP,
    configFiles,
    configAbiFiles,
    langFromURL(url, window.navigator.language),
    stripHref(window.location.href),
    providerType(globEthereum),
    navigator.userAgent,
    process.env.DATA_PROVIDERS,
    );



  function findParent(tagname, el) {
    if (!el) {
      return null;
    }

    if ((el.nodeName || el.tagName).toLowerCase() === tagname.toLowerCase()) {
      return el;
    }

    return findParent(tagname, el.parentNode);
  }

  document.addEventListener('click', (e) => {
    e = e || event;
    var from = findParent('a', e.target || e.srcElement);

    if (from && from.href && !from.rel) {
      history.pushState({}, '', buildHref(from.href));
      e.preventDefault();
      app.ports.onUrlChange.send(from.href);
    }
  });

  // Inform app of browser navigation (the BACK and FORWARD buttons)
  window.addEventListener('popstate', () => {
    app.ports.onUrlChange.send(location.href);
  });

  app.ports.setTitle.subscribe((title) => {
    document.title = title;
  });

  app.ports.handleImageError.subscribe(({ elementId, imgSrc }) => {
    const element = document.getElementById(elementId);

    if (element) {
      console.warn('Image could not be loaded: ', element);

      element.src = imgSrc;
    }
  });
  registerServiceWorker();

  ports.subscribe(
    app,
    globEthereum,
    dataProviders,
    process.env.NETWORK_MAP,
    configAbiFiles,
    process.env.DEFAULT_NETWORK,
    configFiles,
    configAbiFiles,
    configNameToAddressMappings,
    process.env.BLOCKNATIVE_API_KEY
  );
});
