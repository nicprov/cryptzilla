const app = Elm.Main.init({
    flags: JSON.parse(localStorage.getItem('storage'))
})

app.ports.save.subscribe(storage => {
    localStorage.setItem('storage', JSON.stringify(storage))
    app.ports.load.send(storage)
})


app.ports.decryptKeyList.subscribe(function(message) {
    var keyList = message["keyList"]
    window.rclone.Rclone({
        password: message["key"],
        salt: message["salt"]
    }).then(rclone => {
        keyList.keys.forEach(function(key){
            key["keyEncrypted"] = key.key
            key["keyDecrypted"] = rclone.Path.decrypt(key.key);
            delete key.key
        });
        //
        // t = rclone.Path.decrypt("a91kojqe8k000pgm7eu8fj4n50b1hv6r7bqgv0bmunsshkpqpqp0");
        // console.log(t);
        app.ports.decryptedKeyList.send(keyList);
    }).catch(error => {
        console.log(error);
    })
})

app.ports.decryptFile.subscribe(function(message) {
    var file = message["file"]
    window.rclone.Rclone({
        password: message["key"],
        salt: message["salt"]
    }).then(rclone => {

        console.log(file);

        // console.log(nacl.util.decodeUTF8(file))
        //
        // const decodedMessage = nacl.util.decodeUTF8(file)
        //
        // const messageToEncrypt = "test"
        //
        // const generatedKey = nacl.util.encodeBase64(nacl.randomBytes(nacl.secretbox.keyLength));
        // const newNonce = nacl.randomBytes(nacl.secretbox.nonceLength);
        //
        // const e = encrypt(generatedKey, newNonce, messageToEncrypt)
        // console.log(e);
        //
        // const d = decrypt(generatedKey, e);
        // console.log(d);


        // const testEncrypt = encrypt(generatedKey, newNonce, messageToEncrypt)
        // console.log(nacl.util.decodeBase64(testEncrypt))





        messageKey = message["key"]
        console.log(messageKey)



        const key = [
          0x9c, 0x93, 0x5b, 0x48, 0x73, 0x0a, 0x55, 0x4d,
          0x6b, 0xfd, 0x7c, 0x63, 0xc8, 0x86, 0xa9, 0x2b,
          0xd3, 0x90, 0x19, 0x8e, 0xb8, 0x12, 0x8a, 0xfb,
          0xf4, 0xde, 0x16, 0x2b, 0x8b, 0x95, 0xf6, 0x38,
        ]


        // If bad, try with default salt
        const defaultSalt = [0xa8, 0x0d, 0xf4, 0x3a, 0x8f, 0xbd, 0x03, 0x08, 0xa7, 0xca, 0xb8, 0x3e, 0x58, 0x1f, 0x86, 0xb1];

        var scrypt = require('scrypt-js');


        scrypt(reveal(createCipher(), messageKey), defaultSalt, 16384, 8, 1, nacl.secretbox.keyLength, (error, progress, key) => {
            if(error){
                console.log(error);
            }
            if (key) {
                dataKey = new Uint8Array(key.slice(0, 32));



            $.ajax({
                url: "https://nyc3.digitaloceanspaces.com/test-onintime/blnefebla4q99dvg9009vhsjl4",
                method: 'GET',
                xhrFields: { responseType: 'arraybuffer'}
            }).then( (response) => {
                const resp = new Uint8Array(response);
                var messageWithNonceAsUint8Array = resp
                var keyBase64 = nacl.util.encodeBase64(dataKey);
                var messageBase64 = nacl.util.encodeBase64(messageWithNonceAsUint8Array);



                const d = decrypt(keyBase64, messageBase64)
                console.log(d);

            })

            // var messageWithNonceAsUint8Array = decodedMessage
            // var keyBase64 = nacl.util.encodeBase64(dataKey);
            // var messageBase64 = nacl.util.encodeBase64(messageWithNonceAsUint8Array);
            //
            //
            //
            // const d = decrypt(keyBase64, messageBase64)
            // console.log(d);

            // /************************************************
            //  * MAGIC
            //  ************************************************/
            // const magic = messageWithNonceAsUint8Array
            // .subarray(0, 8)
            // // .reduce((acc, i) => acc + String.fromCharCode(i), '');
            //
            // console.log(magic);
            //
            // // Test if this is a valid rClone file
            // if (magic !== 'RCLONE\x00\x00') {
            //     reject(new Error('Magic is wrong'));
            // }
            //
            // /************************************************
            //  * NONCE
            //  ************************************************/
            // const nonce = messageWithNonceAsUint8Array.subarray(8, 32);
            //
            // console.log("Nonce: " + nonce);
            //
            // //
            // // const nonce = messageWithNonceAsUint8Array.slice(0, nacl.secretbox.nonceLength);
            // const message2 = messageWithNonceAsUint8Array.slice(
            //     32,
            //     messageBase64.length
            // );
            //
            // console.log("Message without nonce: " + message2);
            //
            //
            //
            // const decrypted = nacl.secretbox.open(message2, nonce, dataKey);
            // if (!decrypted)
            //     console.log("error");
            // else {
            //     const base64DecryptedMessage = nacl.util.encodeUTF8(decrypted);
            //     console.log(base64DecryptedMessage);
            // }

          }
        });





        // app.ports.decryptedFile.send("test");
        // app.ports.decryptedFile.send(rclone.File.decrypt(file));
    }).catch(error => {
        console.log(error);
    })
})

function encrypt(key, nonce, messageToEncrypt){
    const keyUint8Array = nacl.util.decodeBase64(key);
    const messageUint8 = nacl.util.decodeUTF8(messageToEncrypt);
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
        throw new Error("Magic is wrong");
    }

    const nonce = messageWithNonceAsUint8Array.slice(8, nacl.secretbox.nonceLength + 8);
    const message = messageWithNonceAsUint8Array.slice(
        nacl.secretbox.nonceLength + 8,
        messageWithNonce.length
    );

    const decrypted = nacl.secretbox.open(message, nonce, keyUint8Array);

    if (!decrypted) {
        throw new Error("Could not decrypt message");
    }

    return nacl.util.encodeUTF8(decrypted);
}

function fetchStringFactory(string) {
  return (opts) => {
      return from2(function(size, next) {
    // if there's no more content
    // left in the string, close the stream.
    if (string.length <= 0) return next(null, null)

    // Pull in a new chunk of text,
    // removing it from the string.
    var chunk = string.slice(0, size)
    string = string.slice(size)

    // Emit "chunk" from the stream.
    next(null, chunk)
  }).pipe(chunker(opts.chunkSize, { flush: true }));
  };
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

// This will break for x > 2^53 because Javascript can't
// represent these numbers...
function incrementNonceBy(nonce, x) {
  if (x <= 0) return;
  let carry = 0;
  for (let i = 0; i < 8; i++) {
    const digit = nonce[i];
    const xDigit = x & 0xff;
    x = x >> 8;
    carry = carry + (digit & 0xffff) + (xDigit & 0xffff);
    nonce[i] = carry & 0xff;
    carry = carry >> 8;
  }
  if (carry != 0) {
    incrementNonce(nounce, 8);
  }
}

function incrementNonce(nonce, i = 0) {
  for (; i < nonce.length; i++) {
    const digit = nonce[i];
    nonce[i] = digit + 1;
    if (nonce[i] >= digit) {
      break;
    }
  }
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



// export const decrypt = (messageWithNonce, key) => {
//   const keyUint8Array = decodeBase64(key);
//   const messageWithNonceAsUint8Array = decodeBase64(messageWithNonce);
//   const nonce = messageWithNonceAsUint8Array.slice(0, secretbox.nonceLength);
//   const message = messageWithNonceAsUint8Array.slice(
//     secretbox.nonceLength,
//     messageWithNonce.length
//   );
//
//   const decrypted = secretbox.open(message, nonce, keyUint8Array);
//
//   if (!decrypted) {
//     throw new Error("Could not decrypt message");
//   }
//
//   const base64DecryptedMessage = encodeUTF8(decrypted);
//   return JSON.parse(base64DecryptedMessage);
// };

