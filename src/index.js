import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import '@tinymce/tinymce-webcomponent';
import {setupEditorName, extractEditorContents} from "./tinymce.js";
import loadingPath from '../public/images/loading.svg'
import metadata from './metadata.json'

let app = Elm.Main.init({
    node: document.getElementById('root'),
    flags:
        { setupEditorName: setupEditorName()
        , candorUrl : process.env.ELM_APP_CANDOR_URL
        , loadingPath : loadingPath
        , metadata: metadata
        }
});

function performConsoleLog(theStr) {
    console.log(theStr);
}

app.ports.syncMceEditor.subscribe(extractEditorContents);
app.ports.consoleLog.subscribe(performConsoleLog);

// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

export { app };
