const app = Elm.Main.init({
    flags: JSON.parse(localStorage.getItem('storage'))
})

app.ports.save.subscribe(storage => {
    if (Object.keys(storage).length === 0) { // Sign out event
        localStorage.setItem('storage', JSON.stringify(storage));
        app.ports.load.send(storage);
    }
    else { // Sign in/save settings event
        tmpStorage = JSON.parse(JSON.stringify(storage));
        tmpStorage.encryptionKey = ""; // Don't save encryption key to storage, only keep in memory
        localStorage.setItem('storage', JSON.stringify(tmpStorage));
        app.ports.load.send(storage);
    }
})


app.ports.decryptKeyList.subscribe(function(message) {
    var keys = message["keys"]
    window.rclone.Rclone({
        password: message["password"],
        salt: message["salt"]
    }).then(rclone => {
        keys.forEach(function(key){
            if (key.key.endsWith("/")) {
                key["keyEncrypted"] = key.key
                key["keyDecrypted"] = rclone.Path.decrypt(key.key.substring(0, key.key.length - 1)) + "/";
            } else {
                key["keyEncrypted"] = key.key
                key["keyDecrypted"] = rclone.Path.decrypt(key.key);
            }
            delete key.key
        });
        app.ports.decryptedKeyList.send({keys: keys, error: ""});
    }).catch(error => {
        keys.forEach(function(key){
            key["keyEncrypted"] = ""
            key["keyDecrypted"] = "";
            delete key.key
        });
        app.ports.decryptedKeyList.send({keys: keys, error: error.toString()});
    })
})

app.ports.decryptFile.subscribe(function(message) {
    generateKey(message["key"], message["salt"], (error, key) => {
        if (error != null)
            app.ports.decryptedFile.send(["", error.toString()]);
        else {
            var keyBase64 = nacl.util.encodeBase64(key);
            var messageBase64 = message["file"];
            const r = decrypt(keyBase64, messageBase64)
            app.ports.decryptedFile.send(r);
        }
    });
})

app.ports.encryptFile.subscribe(function(message) {
    generateKey(message["key"], message["salt"], (error, key) => {
        generatedError = "";
        if (error != null)
            generatedError = "Unable to generate key";
        var keyBase64 = nacl.util.encodeBase64(key);
        var messageBase64 = message["file"];
        const e = encrypt(keyBase64, messageBase64)
        app.ports.encryptedFile.send({encryptedFile:e, encryptedPath: rclone.Path.encrypt(message["name"]), error: generatedError});
    });
})

app.ports.encryptFileName.subscribe(function(message) {
    window.rclone.Rclone({
        password: message["key"],
        salt: message["salt"]
    }).then(rclone => {
        app.ports.encryptedFileName.send([rclone.Path.encrypt(message["name"]), ""]);
    }).catch(error => {
        app.ports.encryptedFileName.send(["", error.toString()]);
    })
})
