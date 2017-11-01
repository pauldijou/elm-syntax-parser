const path = require('path')
const Elm = require('../dist/Runner.js')

const app = Elm.Runner.worker()

app.ports.analyze.send(path.resolve(__dirname, 'package', 'elm-package.json'))

app.ports.done.subscribe(project => {
  if (typeof project === 'string') {
    console.error(project)
  } else {
    console.log(require('util').inspect(project, { depth: null }));
  }
})
