![Build & Push to IPFS](https://github.com/compound-finance/palisade/actions/workflows/build-workflow.yml/badge.svg)

# Compound Web3 Front-end

Palisade is the web3 front-end experience to interact with the Compound Ethereum protocol.

## Contributing

We welcome contributions from the community to help keep the Compound web3 front-end working great. You can read more here about [how to contribute](CONTRIBUTING.md).

## Configuration

The web3 front-end requires several items to be configured before it can be started properly. The required format is of the form of several json files that specify config options between local development (`development.json`) and the version that is intended to be deployed (`production.json`). The local and deployment scripts automatically look for those files to exist in the path `config/env`.

```
config/
├── env
│   ├── development.json
│   ├── production.json
```

The following is an example configuration file:

```json
{
  "API_BASE_URL_MAP": {
    "goerli": "https://api.compound.finance/api/",
    "kovan": "https://api.compound.finance/api/",
    "rinkeby": "https://api.compound.finance/api/",
    "ropsten": "https://api.compound.finance/api/",
    "mainnet": "https://api.compound.finance/api/"
  },
  "DATA_PROVIDERS": {
    "development": "http://localhost:8545",
    "goerli": "https://goerli.infura.io/v3/YOUR-PROJECT-ID",
    "rinkeby": "https://rinkeby.infura.io/v3/YOUR-PROJECT-ID",
    "kovan": "https://kovan.infura.io/v3/YOUR-PROJECT-ID",
    "ropsten": "https://ropsten.infura.io/v3/YOUR-PROJECT-ID",
    "mainnet": "https://mainnet.infura.io/v3/YOUR-PROJECT-ID"
  },
  "NETWORK_MAP": {
    "mainnet": 1,
    "ropsten": 3,
    "rinkeby": 4,
    "goerli": 5,
    "kovan": 42,
    "development": 999
  },
  "DEFAULT_NETWORK": "mainnet",
  "BLOCKNATIVE_API_KEY": "YOUR_BLOCKNATIVE_KEY"
}

```

Each of the top level keys have the following functions:

* `API_BASE_URL_MAP` - Object mapping of Eth network name as key and value being the desired Compound Api host. This can be left as is.
* `DATA_PROVIDERS` - Object mapping of Eth network name as key and value being the url of a corresponding JSON RPC host. This example shows Infura as a sample JSON RPC provider and you can find more information [here](https://infura.io/docs/ethereum). Note: this can be specified by setting in the env var `DATA_PROVIDERS` as JSON (e.g. `export DATA_PROVIDERS='{"rinkeby": "https://infura.io/..."}'`).
* `NETWORK_MAP` - Object mapping of Eth network name as key and value being the corresponding NetworkId value. This can be left as is.
* `BLOCKNATIVE_API_KEY` - Blocknative API Key required to track transaction notifications. You can find more information [here](https://docs.blocknative.com/notify). Note: this can be specified by setting the env var `BLOCKNATIVE_API_KEY`. This key is not strictly required (but provides a better user experience).

## Getting Started

The Compound web3 front-end is written in [elm](http://elm-lang.org/) and was bootstrapped with [create elm app](https://github.com/halfzebra/create-elm-app). We strongly recommmend getting familiar with the Elm framework before jumping into the Compound source code.

To get started, first clone this repo:

```bash
> git clone https://github.com/compound-finance/palisade.git && cd palisade
```

Next, install yarn dependencies (note, you should not use `npm` intsead of `yarn` during `install` because `npm` does not respect `yarn.lock` but you should be able to use `npm` for the other commands.):

```bash
> yarn install --lock-file
```

Next, build and watch for string translation changes:

```bash
> yarn watch-i18n
```

Note: For more information on string translations, see [i18n.md](i18n.md)

Next, build and watch for SASS changes:

```bash
> yarn watch-css
```

or, if you prefer just to build your CSS once, run: `yarn build-css`.

And separately start your development server for the front-end:

```bash
> yarn start
```

Note: Elm may take a while to pull in dependencies when you first run the app. At this point you should be able to navigate to [http://localhost:3000](http://localhost:3000) to view your application.

## Deployment

This application is set-up for easy deployment as a static web site.

### Generic Deployment

To deploy this application, first build your static assets:

```bash
> yarn run build-css
> yarn run build
```

Now the `/build` directly should contain all of the files necessary to serve your application from whatever hosting provider you choose. This repo includes support for two options as possible deployment targets, IPFS and Google Cloud Storage.

### IPFS

To deploy the web3 front-end on IPFS, you first should be familiar with [Hosting a single-page website on IPFS](https://docs.ipfs.io/how-to/websites-on-ipfs/single-page-website/). Follow the instructions and you should be able to add all the files in the `/build` directory and obtain a IPFS hash which you can then open on any gateway provider to view the hosted web3 front-end.

Alternatively, you may wish to deploy to an IPFS hosting service like [Infura IPFS](https://infura.io/docs/ipfs#section/Getting-started). This repo includes a script to deploy the `/build` directory to an IPFS host specified by several environment variables.

To deploy a build to Infura IPFS:
```bash
IPFS_AUTH="PROJECT_ID:PROJECT_SECRET" \
  IPFS_HOST="ipfs.infura.io" \
  IPFS_PORT=5001 \
  yarn deploy-ipfs
```

Each of environment variables have the following functions:

* `IPFS_AUTH` - Basic authentication for header for using the Infura IPFS add endpoint. You can find more information [here](https://infura.io/docs/ipfs#section/Authentication).
* `IPFS_HOST` - IPFS Pinning service host.
* `IPFS_PORT` - IPFS Pinning service host port.

Note: The `deploy-ipfs` script has been tested and used with Infura IPFS. You may need a few changes to support alternative pinning services.

### Google Cloud Storage

To deploy the Compound web3 front-end to Google Cloud Storage, you should be familar with [Hosting a Static Site](https://cloud.google.com/storage/docs/hosting-static-website) on Google Cloud. Follow the instructions on creating a CNAME record with your DNS provider and creating a fully-public bucket. Also, make sure to have the [Cloud SDK](https://cloud.google.com/sdk/) tools installed, that you're logged in via `gcloud auth`, and that you have [your correct project set](https://cloud.google.com/sdk/gcloud/reference/config/set).

To deploy to a gcloud bucket:

```bash
> yarn deploy-gcloud your.bucket.name
```

## Internationalization

To learn more about internationalization, please view [i18n.md](i18n.md).

Discussion
----------

For any concerns with the web3 front-end, open an issue or visit us on [Discord](https://discord.com/invite/compound) to discuss.

# License

Copyright 2022, Compound Labs, Inc. and repository contributors. This repository is licensed under GPLv3 (please see [LICENSE](/LICENSE) for the full-text of the license).

All contributors to this repository must release contributed code under this GPLv3 license, free of any other encumbrance. Contributors also agree that contributions may be re-licensed under MIT or BSD-3 licenses in the future without notice. In such instances, all copyright notices will be retained.
