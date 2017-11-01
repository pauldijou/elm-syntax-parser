# elm-syntax-parser

:warning: Experimental

The goal of the project is to provide a nice way to parse all modules of an Elm project and give back information about their types and functions. Right now, it's all in one project but at some point, when more stable, it will be split between the "pure" syntax parser and the "runner" which reads files and stuff like that.

## Try it

```bash
git clone https://github.com/pauldijou/elm-syntax-parser.git
cd elm-syntax-parser
yarn install && yarn deps
yarn build && yarn start
```

You can edit `test/package/src/FakePackage.elm` to see new results.

## License

This software is licensed under the Apache 2 license, quoted below.

Copyright Paul Dijou ([http://pauldijou.fr](http://pauldijou.fr)).

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this project except in compliance with the License. You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0).

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
