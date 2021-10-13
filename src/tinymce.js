import {app} from "./index.js";

window.setupEditor = function(editor) {
    editor.on('dirty', handleDirty);
};
Object.defineProperty(window.setupEditor, 'name', {
    value: 'setupEditor',
    writable: false
});

function setupEditorName() {
    return window.setupEditor.name;
}

function handleDirty() {
    app.ports.dirtyReceived.send(true);
}

// export { setupEditor, handleDirty };
export { setupEditorName };