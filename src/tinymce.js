import {app} from "./index";

export function handleDirty() {
    console.log('Dirty!');
    app.ports.dirtyReceived.send(true);
}
