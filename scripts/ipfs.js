import { create, globSource } from 'kubo-rpc-client';
import { globby } from 'globby';
import { Agent } from 'https';
import { writeFile } from 'fs/promises';

// Ortam değişkenleri için güvenli varsayılanlar ve tip dönüşümleri
const IPFS_CONFIG = {
  host: process.env['IPFS_HOST'],
  port: parseInt(process.env['IPFS_PORT'] || '5001'),
  protocol: process.env['IPFS_SSL'] === 'false' ? 'http' : 'https',
  auth: process.env['IPFS_AUTH'] || "",
};

if (!IPFS_CONFIG.host) {
  console.error("❌ Hata: IPFS_HOST ortam değişkeni ayarlanmamış.");
  process.exit(1);
}

// Authorization header'ı bir kez oluşturulur
const authHeader = `Basic ${Buffer.from(IPFS_CONFIG.auth).toString('base64')}`;

/**
 * IPFS İstemcisini yapılandırır. 
 * keepAlive: true ayarı, ardışık dosya yüklemelerinde TCP el sıkışma yükünü azaltır.
 */
function buildIpfsClient() {
  return create({
    host: IPFS_CONFIG.host,
    port: IPFS_CONFIG.port,
    protocol: IPFS_CONFIG.protocol,
    headers: { authorization: authHeader },
    apiPath: '/api/v0',
    agent: new Agent({
      keepAlive: true, // Performans için true olmalı
      maxSockets: 32,  // Sonsuz yerine makul bir sınır, sistem kaynaklarını korur
    }),
    timeout: '15m' // Büyük build'ler için süre biraz daha artırıldı
  });
}



(async function deploy() {
  try {
    const buildDir = 'build';
    const allFiles = await globby([`${buildDir}/**/*`]);
    const expectedCount = allFiles.length;

    if (expectedCount === 0) {
      throw new Error(`'${buildDir}' dizini boş veya bulunamadı.`);
    }

    console.log(`🚀 ${expectedCount} dosya IPFS'e yüklenmeye hazırlanıyor...`);

    const ipfs = buildIpfsClient();
    const uploadedFiles = [];

    // Dosya yükleme akışı
    for await (const file of ipfs.addAll(globSource(buildDir, '**/*'), { 
      wrapWithDirectory: true,
      pin: true, // Kalıcılık için otomatik pinleme
      progress: (prog) => console.log(`Transfer: ${prog} bytes`)
    })) {
      uploadedFiles.push(file);
      // Sadece önemli dosyaları veya ilerlemeyi logla (gürültüyü azalt)
      if (file.path === "") console.log(`✅ Root CID Oluşturuldu: ${file.cid}`);
    }

    // Doğrulama mantığı: wrapWithDirectory kullanıldığında uploadedFiles.length = expectedCount + 1 (root dizini)
    if (uploadedFiles.length <= expectedCount) {
      throw new Error(`Yükleme eksik kaldı. Beklenen: >${expectedCount}, Yüklenen: ${uploadedFiles.length}`);
    }

    // Root CID her zaman path'i boş ("") olan son nesnedir.
    const rootFolder = uploadedFiles.find(f => f.path === "");
    if (!rootFolder) {
      throw new Error("Root dizini CID'si belirlenemedi.");
    }

    const rootCID = rootFolder.cid.toString();

    // Sonuçları raporla
    const gateways = [
      { name: "IPFS Gateway", url: `https://ipfs.io/ipfs/${rootCID}` },
      { name: "Infura Gateway", url: `https://compound-app.infura-ipfs.io/ipfs/${rootCID}` },
    ];

    console.log("\n🗺  App başarıyla IPFS'e dağıtıldı:");
    gateways.forEach(gw => console.log(`  * ${gw.name}: ${gw.url}`));

    // CID'yi dosyaya yaz (Release takibi için)
    await writeFile('.release', rootCID, 'utf8');
    console.log(`\n💾 Root CID '${rootCID}' .release dosyasına kaydedildi.`);

  } catch (error) {
    console.error("\n❌ Dağıtım sırasında kritik hata oluştu:");
    console.error(error.message);
    process.exit(1);
  }
})();
