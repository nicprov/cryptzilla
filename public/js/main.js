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
            key.key = rclone.Path.decrypt(key.key);
        });
        app.ports.decryptedKeyList.send(keyList);
    }).catch(error => {
        console.log(error);
    })

})

// app.ports.crypt.subscribe(function(message) {
//     r = window.rclone.Rclone({
//         password: message.password,
//         salt: message.salt
//     })
//     switch (message.action){
//         case "encrypt":
//             if (message.messageType === "path")
//                 app.ports.messageReceiver.send(r.then(rclone.Path.encrypt(message)));
//             else
//                 app.ports.messageReceiver.send(r.then(rclone.File.encrypt(message)));
//             break
//         case "decrypt":
//             if (message.messageType === "path")
//                 app.ports.messageReceiver.send(r.then(rclone.Path.decrypt(message)));
//             else
//                 app.ports.messageReceiver.send(r.then(rclone.File.decrypt(message)));
//             break
//         default:
//             break;
//     }
// });

// window.rclone.Rclone({
//     password: '4MHsEgQ21fXF_BwI7CpJFANqL8qq',
//     salt: ''
// })
//     .then(rclone => {
//         //
//         // // Decryption
//         console.log(
//             rclone.Path.decrypt("5kja2jd36pfsqs7mtlrj8m2s9o") // Hello World
//         );
//
//         // Encryption
//         console.log(
//             rclone.Path.encrypt("Hello/World") // dk7voi2247uqbgbuh439j13eo0/p0q5lhi767fsplsdjla7j7uv60
//         );
//
//     })
//     .catch(error => {
//         // Catch error creating rclone instance
//         console.log(error)
//     })