import {app} from "./index.js";

// TinyMCE setup attribute needs to refer to a function in the global namespace, outside
// of any module. Therefore, create the function as a property of the global window object...

var currentEditor = null;

window.setupEditor = function(editor) {
    currentEditor = editor;
    editor.on('dirty', handleDirty);
};

// Since the just created function has an empty string for a name, give it a name
// using the defineProperty function.
Object.defineProperty(window.setupEditor, 'name', {
    value: 'setupEditor',
    writable: false
});

// Now we finally have a string we can use in the setup attribute that TinyMCE can
// find and call :)
function setupEditorName() {
    return window.setupEditor.name;
}

// Fire off the dirtyReceived port by sending true
function handleDirty() {
    app.ports.dirtyReceived.send(true);
}

function extractEditorContents() {
    app.ports.mceEditorSubscription.send(currentEditor.getContent());
}

// export { setupEditor, handleDirty };
export { setupEditorName, extractEditorContents };