const crypto = require('crypto').webcrypto;
const https = require('https');
const fs = require('fs/promises');

async function post(host, path, data, signature) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: host,
      port: 443,
      path: path,
      method: 'POST',
      headers: {
        'Content-Length': data.length,
        'x-signature': signature
      }
    }

    let resp = '';
    let status = null;
    const req = https.request(options, res => {
      status = res.statusCode;

      res.on('data', d => {
        resp += d;
      })
    })

    req.on('error', error => {
      reject(error);
    });

    req.on('close', error => {
      if (status === 200) {
        resolve(JSON.parse(resp));
      } else {
        reject(resp);
      }
    });

    req.write(data);
    req.end();
  });
}

async function sign(cid, ipfsSecret) {
  let enc = new TextEncoder("utf-8");
  let secretEnc = enc.encode(ipfsSecret);

  let key = await crypto.subtle.importKey(
    "jwk",
    JSON.parse(ipfsSecret),
    { name: "ECDSA", namedCurve: "P-384" },
    false,
    ["sign"]
  );

  // Verify CID has been signed
  let signature = await crypto.subtle.sign(
    { name: "ECDSA", hash: {name: "SHA-384"} },
    key,
    enc.encode(cid)
  );

  return Buffer.from(signature).toString('hex');
}

async function release(cid, host, path, ipfsSecret) {
  console.log(`Release cid=${cid}`);
  let signature = await sign(cid, ipfsSecret);
  let res = await post(host, path, cid, signature);

  if (res.cid) {
    console.log(`Successfully released: ${res.cid}`);
  } else {
    throw new Error(`Invalid response: ${JSON.stringify(res)}`);
  }
}

async function run(maybeRelease) {
  const releaseFile = '.release';
  const ipfsSecret = process.env['IPFS_SECRET'];
  const host = 'app-ipfs.compound.finance';
  const path = '/release';

  if (!ipfsSecret) {
    throw new Error("Missing IPFS_SECRET");
  }

  let cid;
  if (maybeRelease) {
    cid = maybeRelease;
  } else {
    cid = (await fs.readFile(releaseFile, 'utf-8')).trim();
  }

  return await release(cid, host, path, ipfsSecret);
}

let [_node, _app, maybeRelease, ...rest] = process.argv;

run(maybeRelease);
