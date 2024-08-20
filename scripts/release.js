const { ethers } = require('ethers');
const fetch = require('node-fetch');
const fs = require('fs/promises');

const releaseFile = '.release';
const ethereumNode = process.env['ETHEREUM_NODE'] || 'http://localhost:8585';
const workerHost = process.env['WORKER_HOST'] || 'https://v2-app.compound.finance';
const url = `${workerHost}/release`;

async function release(cid, url, signature) {
  console.log(`Release cid=${cid}, url=${url}`);

  const res = await fetch(url, {
    body: cid,
    method: 'POST',
    headers: {
      'x-signature': signature,
    },
  });

  try {
    const json = await res.json();
  } catch (error) {
    console.error(`Response was not valid JSON: ${error}`);
  }

  if (json.cid) {
    console.log(`Successfully released: ${json.cid}`);
  } else {
    throw new Error(`Invalid response: ${JSON.stringify(json)}`);
  }
}

async function run(cid, signature) {
  if (!cid) {
    cid = (await fs.readFile(releaseFile, 'utf-8')).trim();
  }

  if (!signature) {
    // If no signature, pull from Ethereum node, e.g. Seacrest
    const provider = new ethers.providers.JsonRpcProvider(ethereumNode);
    const signer = provider.getSigner();
    signature = await signer.signMessage(cid);
  }

  return await release(cid, url, signature);
}

let [_node, _app, cidArg, signatureArg, ...rest] = process.argv;
const cid = cidArg || process.env['CID'];
const signature = signatureArg || process.env['SIGNATURE'];

run(cid, signature);
