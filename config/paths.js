'use strict';

const path = require('path');
const fs = require('fs');
const url = require('url');

// Make sure any symlinks in the project folder are resolved:
// https://github.com/facebookincubator/create-react-app/issues/637
const appDirectory = fs.realpathSync(process.cwd());
const resolveApp = (relativePath) => path.resolve(appDirectory, relativePath);

const envPublicUrl = process.env.PUBLIC_URL;

function ensureSlash(path, needsSlash) {
  const hasSlash = path.endsWith('/');
  if (hasSlash && !needsSlash) {
    return path.substr(path, path.length - 1);
  } else if (!hasSlash && needsSlash) {
    return `${path}/`;
  }
  return path;
}

const getPublicUrl = (appPackageJson) => envPublicUrl || require(appPackageJson).homepage;

// We use `PUBLIC_URL` environment variable or "homepage" field to infer
// "public path" at which the app is served.
// Webpack needs to know it to put the right <script> hrefs into HTML even in
// single-page apps that may serve index.html for nested URLs like /todos/42.
// We can't use a relative path in HTML because we don't want to load something
// like /todos/42/static/js/bundle.7289d.js. We have to know the root.
function getServedPath(appPackageJson) {
  const publicUrl = getPublicUrl(appPackageJson);
  const servedUrl = envPublicUrl || (publicUrl ? url.parse(publicUrl).pathname : '/');
  return ensureSlash(servedUrl, true);
}

module.exports = {
  appPath: resolveApp('.'),
  appPublic: resolveApp('./public'),
  dappHtml: resolveApp('./public/dapp.html'),
  dappIndexJs: resolveApp('./src/dapp.js'),
  appSrc: resolveApp('./src'),
  dotenv: resolveApp('./.env'),
  appBuild: resolveApp('./build'),
  elmJson: resolveApp('./elm.json'),
  elm: require.resolve('elm/bin/elm'),
  publicUrl: getPublicUrl(resolveApp('elm.json')),
  servedPath: getServedPath(resolveApp('elm.json')),
  componentsPath: resolveApp('./node_modules/seahorse-compound-components/src/public'),
};
