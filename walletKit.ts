import { EthereumProvider } from "@walletconnect/ethereum-provider";
import { Web3Wallet } from "@walletconnect/web3wallet";
import { ethers } from "ethers";

let provider: EthereumProvider | null = null;
let signer: ethers.Signer | null = null;

const PROJECT_ID = "180a7164cfa9e5388daf1160841f65a0";

/**
 * Connect wallet using WalletKit / WalletConnect v2
 */
export async function connectWalletKit() {
  if (!PROJECT_ID) throw new Error("PROJECT_ID required");

  // Initialize provider
  provider = await EthereumProvider.init({
    projectId: PROJECT_ID,
    chains: [1, 10, 8453], // Mainnet, Optimism, Base
    showQrModal: true,
  });

  await provider.enable();

  const web3Provider = new ethers.providers.Web3Provider(provider);
  signer = web3Provider.getSigner();
  const address = await signer.getAddress();
  const network = await web3Provider.getNetwork();

  // Event listeners
  provider.on("accountsChanged", (accounts: string[]) => {
    console.log("Accounts changed:", accounts);
  });

  provider.on("chainChanged", (chainId: number) => {
    console.log("Chain changed:", chainId);
  });

  provider.on("disconnect", () => {
    console.log("Disconnected");
    provider = null;
    signer = null;
  });

  // Optional: integrate Web3Wallet (Reown Kit v2)
  const wallet = await Web3Wallet.init({
    projectId: PROJECT_ID,
    metadata: {
      name: "Palisade dApp",
      description: "Demo WalletKit integration",
      url: "https://github.com/compound-finance/palisade",
      icons: ["https://walletconnect.com/walletconnect-logo.png"],
    },
  });

  wallet.on("session_delete", () => console.log("Session deleted"));
  wallet.on("session_expire", () => console.log("Session expired"));

  return { provider, signer, address, network, wallet };
}

/**
 * Disconnect wallet
 */
export async function disconnectWalletKit() {
  if (provider) {
    await provider.disconnect();
    provider = null;
    signer = null;
    console.log("Wallet disconnected");
  }
}
