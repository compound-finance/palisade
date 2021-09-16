'use strict';

const fs = require('fs');
const path = require('path');
const paths = require('./paths');

// Make sure that including paths.js after env.js will read .env variables.
delete require.cache[require.resolve('./paths')];

const NODE_ENV = process.env.NODE_ENV;
if (!NODE_ENV) {
  throw new Error('The NODE_ENV environment variable is required but was not specified.');
}
const CONFIG_ENV = process.env.CONFIG_ENV || NODE_ENV;

// https://github.com/bkeepers/dotenv#what-other-env-files-can-i-use
var dotenvFiles = [
  `${paths.dotenv}.${NODE_ENV}.local`,
  `${paths.dotenv}.${NODE_ENV}`,
  // Don't include `.env.local` for `test` environment
  // since normally you expect tests to produce the same
  // results for everyone
  NODE_ENV !== 'test' && `${paths.dotenv}.local`,
  paths.dotenv,
].filter(Boolean);

// Load environment variables from .env* files. Suppress warnings using silent
// if this file is missing. dotenv will never modify any environment variables
// that have already been set.
// https://github.com/motdotla/dotenv
dotenvFiles.forEach((dotenvFile) => {
  if (fs.existsSync(dotenvFile)) {
    require('dotenv').config({
      path: dotenvFile,
    });
  }
});

// We support resolving modules according to `NODE_PATH`.
// This lets you use absolute paths in imports inside large monorepos:
// https://github.com/facebookincubator/create-react-app/issues/253.
// It works similar to `NODE_PATH` in Node itself:
// https://nodejs.org/api/modules.html#modules_loading_from_the_global_folders
// Note that unlike in Node, only *relative* paths from `NODE_PATH` are honored.
// Otherwise, we risk importing Node.js core modules into an app instead of Webpack shims.
// https://github.com/facebookincubator/create-react-app/issues/1023#issuecomment-265344421
// We also resolve them to make sure all tools using them work consistently.
const appDirectory = fs.realpathSync(process.cwd());
process.env.NODE_PATH = (process.env.NODE_PATH || '')
  .split(path.delimiter)
  .filter((folder) => folder && !path.isAbsolute(folder))
  .map((folder) => path.resolve(appDirectory, folder))
  .join(path.delimiter);

// Grab NODE_ENV and ELM_APP_* environment variables and prepare them to be
// injected into the application via DefinePlugin in Webpack configuration.
const ELM_APP = /^ELM_APP_/i;

// Load valid network configurations
const networksPath = path.join('node_modules/compound-config', 'networks');
const allFiles = fs.readdirSync(networksPath);
const fileRegex = /^([a-z]+)\.json$/;
const fileAbiRegex = /^([a-z]+)\-abi\.json$/;
let networkConfigFiles = getConfigFiles(networksPath, allFiles, fileRegex);
let networkAbiConfigFiles = getConfigFiles(networksPath, allFiles, fileAbiRegex);

// See if we should apply any network config overrides
const overrideNetworksPath = path.join(appDirectory, '/config/networks-override');
if (fs.existsSync(overrideNetworksPath)) {
  const overrideAllFiles = fs.readdirSync(overrideNetworksPath);
  const overriddenNetworkConfigFiles = getConfigFiles(overrideNetworksPath, overrideAllFiles, fileRegex);
  const overriddenNetworkAbiConfigFiles = getConfigFiles(overrideNetworksPath, overrideAllFiles, fileAbiRegex);

  Object.entries(overriddenNetworkConfigFiles).forEach(([overriddenKey, overriddenValue]) => {
    if (networkConfigFiles.hasOwnProperty(overriddenKey)) {
      networkConfigFiles[overriddenKey] = overriddenValue;
    }
  });

  Object.entries(overriddenNetworkAbiConfigFiles).forEach(([overriddenKey, overriddenValue]) => {
    if (networkAbiConfigFiles.hasOwnProperty(overriddenKey)) {
      networkAbiConfigFiles[overriddenKey] = overriddenValue;
    }
  });
}

const envPath = path.join(appDirectory, '/config/env');
const envJson = fs.readFileSync(path.join(envPath, `${CONFIG_ENV}.json`));
const envConfig = JSON.parse(envJson);

if (process.env['BLOCKNATIVE_API_KEY']) {
  envConfig.BLOCKNATIVE_API_KEY = process.env['BLOCKNATIVE_API_KEY'];
}

if (process.env['DATA_PROVIDERS']) {
  try {
    envConfig.DATA_PROVIDERS = JSON.parse(process.env['DATA_PROVIDERS']);
  } catch (e) {
    console.error("");
    console.error("Error: DATA_PROVIDERS provided but is not valid JSON");
    console.error("");
    console.error("");
    throw e;
  }
}

console.log(envConfig);

const configPath = path.join(appDirectory, '/config');

function getConfigFiles(filesPath, allFilesList, fileRegex) {
  return allFilesList.reduce((acc, file) => {
    const match = file.match(fileRegex);

    if (!!match && (process.env.NODE_ENV !== 'production' || match[1] !== 'development')) {
      return {
        ...acc,
        [match[1]]: JSON.parse(fs.readFileSync(path.join(filesPath, file))),
      };
    } else {
      return acc;
    }
  }, {});
}

function getClientEnvironment(publicUrl) {
  const mountPath = process.env.MOUNT_PATH || publicUrl ? new URL(publicUrl).pathname.slice(1) : null;

  const raw = Object.keys(process.env)
    .filter((key) => ELM_APP.test(key))
    .reduce(
      (env, key) => {
        env[key] = process.env[key];
        return env;
      },
      {
        ...envConfig,

        // Useful for determining whether we’re running in production mode.
        NODE_ENV: process.env.NODE_ENV || 'development',
        // Useful for resolving the correct path to static assets in `public`.
        // For example, <img src={process.env.PUBLIC_URL + '/img/logo.png'} />.
        // This should only be used as an escape hatch. Normally you would put
        // images into the `src` and `import` them in code to get their paths.
        PUBLIC_URL: publicUrl,
        CONFIG_FILES: networkConfigFiles,
        CONFIG_ABI_FILES: networkAbiConfigFiles,
        MOUNT_PATH: mountPath,
      }
    );
  // Stringify all values so we can feed into Webpack DefinePlugin
  const stringified = {
    'process.env': Object.keys(raw).reduce((env, key) => {
      env[key] = JSON.stringify(raw[key]);
      return env;
    }, {}),
  };

  return { raw, stringified };
}

module.exports = getClientEnvironment;
