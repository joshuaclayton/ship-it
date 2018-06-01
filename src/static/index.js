require("normalize.css");
require("./styles/main.scss");
import subscribe from "./ElmLocalStoragePorts";

const Elm = require("../elm/Main");

const app = Elm.Main.fullscreen();

subscribe(app);
