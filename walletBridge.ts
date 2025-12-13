import { connectWalletKit, disconnectWalletKit } from "./walletKit";
import { Elm } from "./Main.elm";

const app = Elm.Main.init({ node: document.getElementById("root") });

app.ports.connect.subscribe(async () => {
  try {
    const result = await connectWalletKit();
    console.log("Wallet connected:", result.address);
  } catch (err) {
    console.error("Failed to connect wallet:", err);
  }
});

app.ports.disconnect.subscribe(async () => {
  await disconnectWalletKit();
});
