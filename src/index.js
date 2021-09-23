import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import '@tinymce/tinymce-webcomponent';
import {handleDirty} from "./tinymce";

export var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {setupEditorName: setupEditor.name}
});

function setupEditor(editor) {
  console.log('setupEditor!');
  editor.on('dirty', handleDirty);
}

// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
