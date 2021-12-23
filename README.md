## Getting Started

The Compound web3 front-end is written in [elm](http://elm-lang.org/) and was bootstrapped with [create elm app](https://github.com/halfzebra/create-elm-app). We strongly recommend getting familiar with the Elm framework before jumping into the Compound source code.

Install yarn dependencies. If you are a MAC User, you *must* leave out the `--ignore-optional` flag.

```bash
> yarn install --lock-file --ignore-optional --ignore-platform
```

Next, build translations:

```bash
> yarn i18n
```

Next, build css files:

```bash
> yarn build-css
```

And separately start your development server for the front-end:

```bash
> yarn start
```

Note: Elm may take a while to pull in dependencies when you first run the app. At this point you should be able to navigate to [http://localhost:3000](http://localhost:3000) to view your application.
Make sure to have a running forked blockchain running on port 8545.

### Generic Deployment

To deploy this application, first build your static assets:

```bash
> yarn run build-css
> yarn run build
```

Now the `/build` directly should contain all of the files necessary to serve your application from whatever hosting provider you choose. This repo includes support for two options as possible deployment targets, IPFS and Google Cloud Storage.
