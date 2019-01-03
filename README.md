# GeneWeb

GeneWeb is an open source genealogy software written in OCaml. It comes
with a Web interface and can be used off-line or as a Web service.

## Documentation

The documentation is available online: http://geneweb.tuxfamily.org/

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

## Building the code

### Build status

| [Linux/OSX][lin-link] | [Windows][win-link] |
| :-------------------: | :-----------------: |
| ![lin-badge]          | ![win-badge]        |

[lin-link]:  https://travis-ci.org/geneweb/geneweb "Travis build status"
[lin-badge]: https://travis-ci.org/geneweb/geneweb.svg?branch=master "Travis build status"
[win-link]:  https://ci.appveyor.com/project/ipfix/geneweb "AppVeyor build status"
[win-badge]: https://ci.appveyor.com/api/projects/status/k7e1c67m4hc22491/branch/master?svg=true "AppVeyor build status"

### Build instructions

In June 2018, the build environment of GeneWeb moved to Dune/Jbuilder.
The new configure script returns only the following line:
```
[INFO] Checking OS type ......................................... OsType
```
1. Install OCaml (http://ocaml.org/)
- Install opam (https://opam.ocaml.org/)
- Install GeneWeb (with opam).
- Install additional components. If opam has not proposed to install missing components,
try with apt/apt-get.

2. Clone the repository
```
git clone https://github.com/geneweb/geneweb
```
3. Compile GeneWeb
```
cd geneweb
./configure
make clean; make
make distrib
```

### Building the API

The API uses the Google Protocol Buffer to exchange information
encoded as pb, json, xml.

It has the following dependencies:

- OCaml, camlp5, lwt, ocamlfind, ocurl, piqi, piqilib, protobuf, re, uuidm, yojson, redis

### Docker

You can use Docker to build and run geneweb on any computer.
```
git clone https://github.com/geneweb/geneweb
cd geneweb
docker build -t geneweb .
docker run --rm -it -p 2316:2316 -p 2317:2317 -v `pwd`:/home/opam/geneweb/ -v YOUR_BASE_DIR:/home/opam/bases/ geneweb /home/opam/geneweb/docker/build.sh --run --api --clean
```
The following options are available for the build script:
- run: runs geneweb and gwsetup right after build
- clean: runs `make clean` before build phase
- api: builds the api

## Contributor guidelines

### Creating a pull request

Use the bug number/title as the name of pull request. Example of
commit message "PR #XXX: Title of the PR/Bug".

### Coding style

* Try to keep the same coding style as the existing one.
* New code should not contain any trailing whitespace.
* Each commit should have a single clear purpose. If a commit contains
  multiple unrelated changes, those changes should be split into
  separate commits.
* If a commit requires another commit to build properly, those commits
  should be squashed.
* If the PR needs to be update, push force.

## CONTENTS

|   File    |                  Description                            |
| --------- | ------------------------------------------------------- |
| CHANGES   | changes (for genealogists non programmers)              |
| ICHANGES  | changes (for programmers)                               |
| LICENSE   | license notice                                          |
| configure | configure script                                        |
| Makefile  | main Makefile                                           |
| README    | this file                                               |
| INSTALL   | installation file                                       |
| contrib   | users contributions                                     |
| dag2html  | library to create html trees                            |
| etc       | additional files for distribution                       |
| ged2gwb   | converter GEDCOM -> GeneWeb                             |
| gwb2ged   | converter GeneWeb -> GEDCOM                             |
| gwtp      | CGI to upload and download databases                    |
| hd        | html and image files for GeneWeb program                |
| rpm       | files to create Linux rpm packages                      |
| setup     | service (gwsetup) to launch commands in a Web navigator |
| src       | sources of main programs                                |
| tools     | tools for compiling                                     |
| wserver   | library for creating Web services                       |


After June 2018, source files have been distributes across three folders:

|   File    |                  Description                            |
| --------- | ------------------------------------------------------- |
| internal  | sources of tools                                        |
| lib       | sources of server components                            |
| src       | sources of main programs                                |

## COPYRIGHT

All files marked in this distribution are Copyright (c) 1998-2016 INRIA
(Institut National de Recherche en Informatique et Automatique) and
distributed under the conditions stated in file LICENSE. They can be
freely redistributed for non-commercial purposes, provided the
copyright notice remains attached.

## INSTALLATION

You can compile on Unix, Windows or Mac OS X machines.
See the file INSTALL for installation instructions.
