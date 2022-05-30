const ipfsClient = require('ipfs-http-client');
const { Agent } = require('https');
const { writeFile } = require('fs/promises');

const ipfsAuth = process.env['IPFS_AUTH'] || "";
const ipfsHost = process.env['IPFS_HOST'];
const ipfsPort = process.env['IPFS_PORT'] ? parseInt(process.env['IPFS_PORT']) : 5001;
const ipfsProtocol = process.env['IPFS_SSL'] === 'false' ? 'http' : 'https';

if (!ipfsHost) {
  console.error("Must set IPFS_HOST");
  process.exit(1);
}

function buildIpfsClient() {
  return ipfsClient.create({
    host: ipfsHost,
    port: ipfsPort,
    protocol: ipfsProtocol,
    headers: {
      authorization: ipfsAuth
    },
    apiPath: '/api/v0',
    agent: new Agent({
      keepAlive: false,
      maxSockets: Infinity
    })
  });
}

(async function() {
  let ipfs = buildIpfsClient();
  function progress(size, path) {
    console.log(`Sent ${Math.round(size / 1000)}KB for ${path}`);
  }
  let app = await ipfs.add(ipfsClient.globSource('build', { recursive: true, progress }));
  if (app === null) {
    throw new Error("Missing core application cid");
  }
  console.log(`Pushed ${app.path} [size=${app.size}, cid=${app.cid}]`);

  urls = [
    ["IPFS Url", `https://ipfs.io/ipfs/${app.cid}`],
    ["Cloudflare Url", `https://cloudflare-ipfs.com/ipfs/${app.cid}`],
    ["Infura Url", `https://ipfs.infura.io/ipfs/${app.cid}`],
  ];
  urlText = urls.map(([name, url]) => `  * ${name}: ${url}`).join("\n");

  console.log("\n\n");
  console.log("🗺  App successfully deployed to ipfs:\n");
  console.log(urlText);
  console.log("\n");

  writeFile('.release', `${app.cid}`, 'utf8');
})();
