const fs = require('fs');
const sysPath = require('path');

const srcDir = sysPath.join(__dirname, '..', 'tests', 'SHA-RSP');
const dstDir = sysPath.join(__dirname, '..', 'tests', 'SHA');

if (!fs.existsSync(dstDir)) {
  fs.mkdirSync(dstDir);
}

const parseSource = (srcName) => {
  let lines = fs.readFileSync(sysPath.join(srcDir, srcName))
    .toString()
    .split('\n')
    .slice(7);

  const vectors = [];

  while (true) {
    const [len, msg, md] = lines;
    lines = lines.slice(4);

    if (len.length === 0) { break; }

    vectors.push({
      msg: msg.replace(/^Msg = (.*?)(\r|\n)*$/, '$1'),
      md: md.replace(/^MD = (.*?)(\r|\n)*$/, '$1'),
    });
  }

  return vectors;
}

const format = ({ msg, md }) =>
  `( "${md}", "${msg === '00' ? '' : msg}" )`

const convert = (srcName) => {
  const vectors = parseSource(srcName);
  const name = srcName.replace(/\.rsp$/, '');
  const dst = sysPath.join(dstDir, `${name}.elm`);
  fs.writeFileSync(dst, `module SHA.${name} exposing (vectors)


vectors : List ( String, String )
vectors =
    [ ${vectors.map(format).join('\n    , ')}
    ]
`);
  console.log(dst);
};

const files = fs.readdirSync(srcDir)
  .filter(name => name.endsWith('.rsp'))
  .forEach(convert);
