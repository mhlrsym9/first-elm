import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import '@tinymce/tinymce-webcomponent';
import {setupEditorName} from "./tinymce.js";

let app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: {setupEditorName: setupEditorName() }
});

// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

export { app };
