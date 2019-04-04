# GeneWeb

GeneWeb is an open source genealogy software written in OCaml. It comes
with a Web interface and can be used off-line or as a Web service.

## Build status

| [Linux/OSX][lin-link] | [Windows][win-link] |
| :-------------------: | :-----------------: |
| ![lin-badge]          | ![win-badge]        |

[lin-link]:  https://travis-ci.org/geneweb/geneweb "Travis build status"
[lin-badge]: https://travis-ci.org/geneweb/geneweb.svg?branch=master "Travis build status"
[win-link]:  https://ci.appveyor.com/project/ipfix/geneweb "AppVeyor build status"
[win-badge]: https://ci.appveyor.com/api/projects/status/k7e1c67m4hc22491/branch/master?svg=true "AppVeyor build status"

## Documentation

The documentation is available online: http://geneweb.tuxfamily.org/

## Installation

Using [opam](https://opam.ocaml.org/):

```
opam pin add -k git https://github.com:geneweb/geneweb --no-action
opam depext geneweb
opam install geneweb
```

## Getting involved

We encourage you to participate in this open source project. We love
pull requests, bugs reports, ideas...

* Documentation: http://geneweb.tuxfamily.org/wiki/GeneWeb
* Mailing list: https://groups.yahoo.com/neo/groups/GeneWeb/info
* IRC: irc://irc.freenode.net/geneweb
* Git: https://github.com/geneweb/geneweb
* Forum: http://www.geneanet.org/forum/GeneWeb-85
* Facebook group: http://www.facebook.com/geneweb
* Wikipedia: https://en.wikipedia.org/wiki/GeneWeb

## Contributor guidelines

### Install dependencies

```
$ opam install camlp5 cppo dune markup ounit
```

### Build instructions

1. Run the configuration script
   ```
   $ ./configure
   ```
2. Build the distibution
   ```
   $ make clean distrib
   ```

### Building the API

The API uses the Google Protocol Buffer to exchange information
encoded as pb, json, xml.

Install [protoc](https://github.com/protocolbuffers/protobuf#protocol-compiler-installation),
and these extra dependencies:

```
$ opam install ocurl piqi piqilib redis redis-sync yojson
```

Then, tell the configure script to enable API.

```
$ ./configure --api
```

### Coding style

* Try to keep the same coding style as the existing one.
* New code should not contain any trailing whitespace.
* If a unit test can be written
* Each pull request should have a single clear purpose. If it containes
  multiple unrelated changes, those changes should be split into
  separate pull requests.
* If the PR needs to be update, push force.
* When submitting a new feature/function, write documentation and tests if it is relevant.

## Copyright

All files marked in this distribution are Copyright (c) 1998-2016 INRIA
(Institut National de Recherche en Informatique et Automatique) and
distributed under the GNU GENERAL PUBLIC LICENSE. See [LICENSE](LICENSE) file
for details.
