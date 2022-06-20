import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import '@tinymce/tinymce-webcomponent';
import {setupEditorName, extractEditorContents, editorConfigName, setDirtySlideTextFlag} from "./tinymce.js";
import loadingPath from '../public/images/loading.svg'
import metadata from './metadata.json';
import '@github/clipboard-copy-element';

let app = Elm.Main.init({
    node: document.getElementById('root'),
    flags:
        {
            setupEditorName: setupEditorName(),
            editorConfigName : editorConfigName(),
            candorUrl : process.env.ELM_APP_CANDOR_URL,
            loadingPath : loadingPath,
            metadata: metadata,
            seeds : Array.from(crypto.getRandomValues(new Uint32Array(4)))
        }
});

function performConsoleLog(theStr) {
    console.log(theStr);
}

app.ports.syncMceEditor.subscribe(extractEditorContents);
app.ports.updateDirtySlideTextFlag.subscribe(setDirtySlideTextFlag);
app.ports.consoleLog.subscribe(performConsoleLog);

// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

export { app };
