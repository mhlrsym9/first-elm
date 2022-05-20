import {app} from "./index.js";

// TinyMCE setup attribute needs to refer to a function in the global namespace, outside
// of any module. Therefore, create the function as a property of the global window object...

var currentEditor = null;

window.setupEditor = function(editor) {
    if (null != currentEditor) {
        currentEditor.on('dirty', null);
    }
    currentEditor = editor;
    editor.on('dirty', handleDirty);
};

window.editorConfig = {
    content_style: "@import url('https://lds.transparent.com/font/get/ttf/ENGLISH'); body { font-family: Latin; }",
    font_formats: "Andale Mono=andale mono,times; Arial=arial,helvetica,sans-serif; Arial Black=arial black,avant garde; Book Antiqua=book antiqua,palatino; Comic Sans MS=comic sans ms,sans-serif; Courier New=courier new,courier; Georgia=georgia,palatino; Helvetica=helvetica; Impact=impact,chicago; Latin=latin; Symbol=symbol; Tahoma=tahoma,arial,helvetica,sans-serif; Terminal=terminal,monaco; Times New Roman=times new roman,times; Trebuchet MS=trebuchet ms,geneva; Verdana=verdana,geneva; Webdings=webdings; Wingdings=wingdings,zapf dingbats",
    menu: {
        edit: { title: 'Edit', items: 'undo redo | cut copy paste' },
        view: { title: 'View', items: 'code | visualaid' },
        insert: { title: 'Insert', items: 'image link media inserttable | anchor' },
        format: { title: 'Format', items: 'bold italic underline strikethrough superscript subscript codeformat | formats blockformats fontformats fontsizes align lineheight | forecolor backcolor | removeformat' },
        tools: { title: 'Tools', items: 'code' },
        table: { title: 'Table', items: 'inserttable | cell row column | tableprops deletetable' }
    }
}

// Since the just created function has an empty string for a name, give it a name
// using the defineProperty function.
Object.defineProperty(window.setupEditor, 'name', {
    value: 'setupEditor',
    writable: false
});

Object.defineProperty(window.editorConfig, 'name', {
    value: 'editorConfig',
    writable: false
});

// Now we finally have a string we can use in the setup attribute that TinyMCE can
// find and call :)
function setupEditorName() {
    return window.setupEditor.name;
}

function editorConfigName() {
    return window.editorConfig.name;
}

// Fire off the dirtyReceived port by sending true
function handleDirty() {
    app.ports.dirtyReceived.send(currentEditor.isDirty());
}

function extractEditorContents() {
    app.ports.mceEditorSubscription.send(currentEditor.getContent());
}

function setDirtyFlag(flag) {
    currentEditor.setDirty(flag);
    app.ports.dirtyReceived.send(currentEditor.isDirty());
}

// export { setupEditor, handleDirty };
export { setupEditorName, editorConfigName, extractEditorContents, setDirtyFlag };