import "./style.css";
import { Main } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";
import ClipboardJS from "../node_modules/clipboard/dist/clipboard.min.js";

var clipboard = new ClipboardJS(".copy-button");

Main.embed(document.getElementById("root"));

registerServiceWorker();
