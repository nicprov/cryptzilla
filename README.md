# cryptzilla
File browser for S3-compliant APIs with files/directories encrypted using NACL (rclone crypt). The file browser is 100% client-based, all encryption/decryption is done directly in the browser, and your data stays safe with you. 

## Compiling
The file browser is written in elm, so you must have it installed. This can be done using npm:
> npm install -g elm

Next, you need to install elm-spa, this can also be done using npm:
> npm install -g elm-spa

Finally, compiling is easy and can be done with a single command:
> elm-spa build

This will generate a compiled version which can be found here: public/dist/elm.js. You can take this file and place it in the public/js repository.


## Template

The base template used was taken from FileGator as can be found here: https://filegator.io/