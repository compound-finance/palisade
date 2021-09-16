import { giveNewTrx, withTrxWeb3, withWeb3Eth } from '../../node_modules/compound-components/src/js/sharedEth/eth';
import { networkFromId } from '../../node_modules/compound-components/src/js/sharedEth/utils';

export function subscribeToRepl(app, eth, configFiles, configAbiFiles, showAccount) {
  let scriptPromise = null;
  let worldResolve;
  let worldPromise = new Promise((resolve, reject) => {
    worldResolve = resolve;
  });

  function loadRepl(src) {
    if (scriptPromise) {
      return scriptPromise;
    } else {
      scriptPromise = new Promise(function (resolve, reject) {
        let script = document.createElement('script');
        script.onload = function () {
          resolve();
        };
        script.src = src;
        document.head.appendChild(script);
      });

      return scriptPromise;
    }
  }

  app.ports.replSetAccount.subscribe((account) => {
    worldPromise = worldPromise.then((world) => {
      // Set default account
      return world.setIn(['accounts', 'default'], { name: 'default', address: account });
    });
  });

  app.ports.replEval.subscribe((line) => {
    if (line.trim().startsWith('\\')) {
      let els = line.split(/\s+/);
      switch (els[0].toLowerCase().slice(1)) {
        case 'account':
          let account = els[1];
          showAccount(app, eth, account);
          app.ports.replPrint.send(`Connecting to ${account}...`);
          break;
        default:
          app.ports.replPrint.send(`
            Commands:
              \\account <account>: Show app as given account
          `);
      }
    } else {
      worldPromise.then((world) => {
        let lastActionCount = world.actions.length;

        worldPromise = webParse(world, line)
          .then(function (newWorld) {
            app.ports.replEnable.send(true);

            let newActions = newWorld.actions.slice(lastActionCount);

            const actions = Promise.all(
              newActions.map((action) => {
                app.ports.replPrint.send(action.log);

                return giveNewTrx(
                  app,
                  eth,
                  action.invokation.receipt.to,
                  action.invokation.receipt.to,
                  world.web3.utils.toChecksumAddress(action.invokation.receipt.from),
                  action.invokation.receipt.transactionHash,
                  action.invokation.method,
                  action.invokation.args.map((arg) => arg.val)
                );
              })
            );

            return actions.then((_) => {
              return newWorld;
            });
          })
          .catch((err) => {
            console.error(err);
            let msg = err.toString();

            app.ports.replPrint.send(msg);
            app.ports.replEnable.send(true);

            return world;
          });
      });
    }
  });

  function logMsg(msg, format) {
    if (format.markdown) {
      app.ports.replMarkdown.send(msg);
    } else {
      app.ports.replPrint.send(msg);
    }
  }

  app.ports.replInit.subscribe(async (msg) => {
    await loadRepl('./js/scenario.js');

    const web3 = await withTrxWeb3(
      eth,
      async (trxWeb3) => trxWeb3,
      async () => {
        await withWeb3Eth(eth);
      }
    );

    ethereum.autoRefreshOnNetworkChange = true;

    const networkId = await web3.net.getId();
    const networkType = networkFromId(networkId);

    webWorld(web3, JSON.stringify(configFiles[networkType]), JSON.stringify(configAbiFiles[networkType]), logMsg).then(
      (world) => {
        worldResolve(world);
      }
    );
  });
}
