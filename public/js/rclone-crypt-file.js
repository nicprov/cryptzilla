// CONSTANTS
const fileMagic = 'RCLONE\x00\x00';
const fileMagicSize = fileMagic.length;
const fileNonceSize = 24;
const fileHeaderSize = fileMagicSize + fileNonceSize;
const blockHeaderSize = 16;
const blockDataSize = 64 * 1024;
const blockSize = blockHeaderSize + blockDataSize;

function encrypt(key, messageToEncrypt){
    const initialNonce = nacl.randomBytes(nacl.secretbox.nonceLength);
    const keyUint8Array = nacl.util.decodeBase64(key);
    const messageUint8 = nacl.util.decodeBase64(messageToEncrypt);

    // Encrypt by chunk of 65552 bytes (65536 bytes + 16 byte MAC)
    const nonce = initialNonce.slice();
    const numBlocks = Math.ceil(messageUint8.length / blockDataSize); // Calculate the number of blocks, since there is no header yet, we use blockDataSize instead of blockSize
    const encryptedSize = ((numBlocks - 1) * blockSize) + ((messageUint8.length % blockDataSize) + blockHeaderSize); // Calculate the total encrypted size, the last block is not a full size hence the modulo
    const encryptedMessage = new Uint8Array(encryptedSize);
    let currentBlock = 0;
    while (currentBlock < numBlocks) {
        if (currentBlock === numBlocks - 1)
            part = messageUint8.slice(currentBlock * blockDataSize);
        else
            part = messageUint8.slice(currentBlock * blockDataSize, (currentBlock + 1) * blockDataSize)
        const encryptedPart = nacl.secretbox(part, nonce, keyUint8Array);
        encryptedMessage.set(encryptedPart, currentBlock * blockSize);
        incrementNonce(nonce); // each part has a different nonce, the initial nonce is just used for the first chunk
        currentBlock += 1;
    }
    const fileMagic = new Uint8Array([82, 67, 76, 79, 78, 69, 0, 0]); // RCLONE\x00\x00
    const fullMessage = new Uint8Array(fileMagic.length + initialNonce.length + encryptedMessage.length);
    fullMessage.set(fileMagic);
    fullMessage.set(initialNonce, fileMagic.length);
    fullMessage.set(encryptedMessage, initialNonce.length + fileMagic.length);
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

    const initialNonce = messageWithNonceAsUint8Array.slice(8, nacl.secretbox.nonceLength + 8);
    const message = messageWithNonceAsUint8Array.slice(
        nacl.secretbox.nonceLength + 8,
        messageWithNonce.length
    );

    // Decrypt by chunk of 65552 bytes (65536 bytes + 16 byte MAC)
    const nonce = initialNonce.slice();
    const numBlocks = Math.ceil(message.length / blockSize); // Calculate the number of blocks
    const decryptedSize = ((numBlocks - 1) * blockDataSize) + ((message.length % blockSize) - blockHeaderSize); // Calculate the total decrypted size, the last block is not a full size hence the modulo
    const decryptedMessage = new Uint8Array(decryptedSize);
    let currentBlock = 0;
    while (currentBlock < numBlocks) {
        if (currentBlock === numBlocks - 1)
            part = message.slice(currentBlock * blockSize);
        else
            part = message.slice(currentBlock * blockSize, (currentBlock + 1) * blockSize)
        const decryptedPart = nacl.secretbox.open(part, nonce, keyUint8Array);
        decryptedMessage.set(decryptedPart, currentBlock * (blockSize - blockHeaderSize));
        if (!decryptedPart) {
            return [ "", "Could not decrypt message" ];
        }
        incrementNonce(nonce); // each part has a different nonce, the initial nonce is just used for the first chunk
        currentBlock += 1;
    }
    return [ nacl.util.encodeBase64(decryptedMessage), "" ]
}

function generateKey(password, salt, callback){
    var scrypt = require('scrypt-js');

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
