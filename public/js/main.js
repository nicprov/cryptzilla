const app = Elm.Main.init({
    flags: JSON.parse(localStorage.getItem('storage'))
})

app.ports.save.subscribe(storage => {
    localStorage.setItem('storage', JSON.stringify(storage))
    app.ports.load.send(storage)
})


app.ports.decryptKeyList.subscribe(function(message) {
    keyList = message["keyList"]
    window.rclone.Rclone({
        password: message["key"],
        salt: message["salt"]
    }).then(rclone => {
        keyList.keys.forEach(function(key){
            key["keyEncrypted"] = key.key
            key["keyDecrypted"] = rclone.Path.decrypt(key.key);
            delete key.key
        });
        console.log(keyList);
        app.ports.decryptedKeyList.send(keyList);
    }).catch(error => {
        console.log(error);
    })

})