var fs = require('fs');

//const { Elm }  = require('./Top.elm'); -- do it this way if using webpack-loader
const {
  Elm
} = require('./elm.js');

const app = Elm.Top.init();

fs.readFile('data/cust.csv', 'utf8', function(err, contents) {
  app.ports.custDataPort.send(contents);
});

fs.readFile('data/acc.csv', 'utf8', function(err, contents) {
  app.ports.accDataPort.send(contents);
});

fs.readFile('data/txn.csv', 'utf8', function(err, contents) {
  app.ports.txnDataPort.send(contents);
});

app.ports.userFilePort.subscribe(request => {
  console.log(request);
  fs.writeFile('users.json', request, (err) => {
    if (err) throw err;
  })
});

app.ports.accountFilePort.subscribe(request => {
  console.log(request);
  fs.writeFile('accounts.json', request, (err) => {
    if (err) throw err;
  })
});

app.ports.txFilePort.subscribe(request => {
  console.log(request);
  fs.writeFile('batch.json', request, (err) => {
    if (err) throw err;
  })
});

app.ports.userIdsFilePort.subscribe(request => {
  console.log(request);
  fs.writeFile('cins.txt', request, (err) => {
    if (err) throw err;
  })
});
