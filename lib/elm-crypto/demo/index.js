require('./index.html');

const { Main } = require('./Main.elm');

Main.embed(document.getElementById('main'));
