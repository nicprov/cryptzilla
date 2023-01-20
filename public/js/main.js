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
    window.rclone.Rclone({
        password: message["key"],
        salt: message["salt"]
    }).then(rclone => {
        generateKey(message["key"], message["salt"], (error, key) => {
            generatedError = "";
            if (error != null)
                generatedError = "Unable to generate key";
            var keyBase64 = nacl.util.encodeBase64(key);
            var messageBase64 = message["file"];
            const e = encrypt(keyBase64, messageBase64)
            app.ports.encryptedFile.send({encryptedFile:e, encryptedPath: rclone.Path.encrypt(message["name"]), error: generatedError});
        });
    }).catch(error => {
        app.ports.encryptedFile.send({encryptedFile:"", encryptedPath: "", error: error.toString()});
    })
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

function encrypt(key, messageToEncrypt){
    const nonce = nacl.randomBytes(nacl.secretbox.nonceLength);
    const keyUint8Array = nacl.util.decodeBase64(key);
    const messageUint8 = nacl.util.decodeBase64(messageToEncrypt);
    const box = nacl.secretbox(messageUint8, nonce, keyUint8Array);
    const fileMagic = new Uint8Array([82, 67, 76, 79, 78, 69, 0, 0]); // RCLONE\x00\0x00
    const fullMessage = new Uint8Array(fileMagic.length + nonce.length + box.length);
    fullMessage.set(fileMagic);
    fullMessage.set(nonce, fileMagic.length);
    fullMessage.set(box, nonce.length + fileMagic.length);
    return nacl.util.encodeBase64(fullMessage);
}

function decrypt(key, messageWithNonce){
    const keyUint8Array = nacl.util.decodeBase64(key);
    const messageWithNonceAsUint8Array = nacl.util.decodeBase64(messageWithNonce);
    const magic = messageWithNonceAsUint8Array
    .subarray(0, 8)
    .reduce((acc, i) => acc + String.fromCharCode(i), '');

    // Test if this is a valid rClone file
    if (magic !== 'RCLONE\x00\x00') {
        return [ "", "Magic is wrong" ]
    }

    const nonce = messageWithNonceAsUint8Array.slice(8, nacl.secretbox.nonceLength + 8);
    const message = messageWithNonceAsUint8Array.slice(
        nacl.secretbox.nonceLength + 8,
        messageWithNonce.length
    );

    const decrypted = nacl.secretbox.open(message, nonce, keyUint8Array);
    if (!decrypted) {
        return [ "", "Could not decrypt message" ];
    }

    return [ nacl.util.encodeBase64(decrypted), "" ]
}

function generateKey(password, salt, callback){
    var scrypt = require('scrypt-js');

    const key = [
      0x9c, 0x93, 0x5b, 0x48, 0x73, 0x0a, 0x55, 0x4d,
      0x6b, 0xfd, 0x7c, 0x63, 0xc8, 0x86, 0xa9, 0x2b,
      0xd3, 0x90, 0x19, 0x8e, 0xb8, 0x12, 0x8a, 0xfb,
      0xf4, 0xde, 0x16, 0x2b, 0x8b, 0x95, 0xf6, 0x38,
    ]

    // If bad, try with default salt
    const defaultSalt = [0xa8, 0x0d, 0xf4, 0x3a, 0x8f, 0xbd, 0x03, 0x08, 0xa7, 0xca, 0xb8, 0x3e, 0x58, 0x1f, 0x86, 0xb1];

    var generatedSalt;

    if (salt === "")
        generatedSalt = defaultSalt;
    else
        generatedSalt = reveal(salt)

    scrypt(reveal(createCipher(), password), generatedSalt, 16384, 8, 1, nacl.secretbox.keyLength, (error, progress, key) => {
        if(error){
            callback(error, null);
        }
        if (key) {
            callback(null, createKey(key));
        }
    });
}

function createKey(key) {
  return new Uint8Array(key.slice(0, 32));
}

function base64(s) {
  if (s.length % 4 != 0) {
    s += '===='.substr(0, 4 - s.length % 4);
  }
  return new Uint8Array(
    atob2(s)
      .split('')
      .map(charCodeAt)
  );
}

function atob2(data) {
  return typeof atob === 'function'
    ? atob(data)
    : Buffer.from(data, 'base64').toString('binary');
}

function charCodeAt(c) {
  return c.charCodeAt(0);
}

function calculateDecryptedSize(size) {
  size = size - fileHeaderSize;
  const blocks = Math.floor(size / blockSize);
  const decryptedSize = blocks * blockDataSize;
  let residue = size % blockSize;
  if (residue !== 0) {
    residue -= blockHeaderSize;
  }
  return decryptedSize + residue;
}

function createCipher() {
    var aes = require('aes-js');

    const {ctr} = aes.ModeOfOperation

    let ctrCipher; // Singelton

    const key = [
      0x9c, 0x93, 0x5b, 0x48, 0x73, 0x0a, 0x55, 0x4d,
      0x6b, 0xfd, 0x7c, 0x63, 0xc8, 0x86, 0xa9, 0x2b,
      0xd3, 0x90, 0x19, 0x8e, 0xb8, 0x12, 0x8a, 0xfb,
      0xf4, 0xde, 0x16, 0x2b, 0x8b, 0x95, 0xf6, 0x38,
    ]

    if (ctrCipher) return ctrCipher;
      // Key extracted from rclone
      return (ctrCipher = new ctr(key, 0));
}

function reveal(ctrCipher, cipherText) {
  const blockCipher = createCipher();

  cipherText = cipherText.replace(/-/g, '+').replace(/_/g, '/');
  const bytes = base64(cipherText);

  const iv = bytes.subarray(0, 16);
  const buf = bytes.subarray(16);

  // I don't always want to create a new instance of
  // AES so I am reusing the ctr cipher and reseting it
  ctrCipher._counter._counter = iv;
  ctrCipher._remainingCounter = null;
  ctrCipher._remainingCounterIndex = 16;

  return ctrCipher.decrypt(buf);
}

