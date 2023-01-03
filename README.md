***
<h1 align="center">
<img src="https://cdn.onintime.com/cryptzilla/logo/logo_white.png" alt="CryptZilla" height="60">
</h1>

***

## Description
CryptZilla is a file manager for S3-compliant APIs with files/directories encrypted using [NaCl Secretbox](https://rclone.org/crypt/) (rclone crypt). The file browser is 100% client-based, all encryption/decryption is done directly in the browser, and your data stays safe with you. 

## Compiling
The file browser is written in elm, so you must have it installed. This can be done using npm:
> npm install -g elm

Next, you need to install elm-spa, this can also be done using npm:
> npm install -g elm-spa

Finally, compiling is easy and can be done with a single command:
> elm-spa build

This will generate a compiled version which can be found here: public/dist/elm.js. You can take this file and place it in the public/js repository.


## Template

The base template used was taken from FileGator which can be found here: https://filegator.io/

## FAQ

### Why did you make this?

I was initially looking for a platform-agnostic, secure, and open-source alternative to Mega where I hold the encryption keys. Rclone is great at what it does - syncing (and encrypting). The issue is that I wanted the full experience that Mega offered, not just a desktop client, but also a great file manager with seemless encryption/decryption in the browser so that I can view/manage my files on the go as well.

When you encrypt your files yourself (as opposed to using a built-in solution like KMS), you have the advantage of holding your keys, but lose some of the benefits that a platform may offer you (like a file manager). I wanted the best of both worlds so I built this.

### Can I host this myself?

Absolutely! You can download the compiled version directly from the [release page](https://github.com/nicprov/cryptzilla/releases). Alternatively, if you don't trust me, no problem, you can easily compile it yourself! I've included the instructions above. I wanted the best of both worlds, so I built this.

### Is it safe to directly use cryptzilla.xyz?

The website is there for demonstration purposes only. While your data should be safe as it never leaves your browser, I do not want to be responsible if anything happens. I strongly recommend that you host it yourself.

### Do I need a server to host CryptZilla?

No! Since CryptZilla is 100% client-based and composed strictly of HTML/CSS/JS files (compiled from elm), you can even run it directly from your computer by opening the "index.html" file.

### Are my credentials stored safely in the browser?

Absolutely! In fact, you are forced to encrypt them when you initially log in. You can view this for yourself by opening the developer console and navigating to Application->Local Storage and clicking on the website. You will notice that the rclone password, rclone salt as well as the access key and secret key are all encrypted using AES.

