# node2nix-hs

[![Hackage](https://img.shields.io/hackage/v/node2nix-hs.svg?logo=haskell)](https://hackage.haskell.org/package/node2nix-hs)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![built with garnix](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fptitfred%2Fnode2nix-hs)](https://garnix.io)

Partial drop&replace reimplementation of node2nix

Supports part of the original node2nix CLI options with notable differences:
- default value for `--composition` is now `node-dependencies.nix` and not `default.nix`
- short options  `-16` and `-18` are not supported
- node version up to 14 are not supported
- node version 20 is supported
- default node version is now 18

Following options are not implemented:
- option `--input`
- option `--development`
- option `--supplement-input`
- option `--supplement-output`
- flag `--include-peer-dependencies`
- flag `--no-flatten`
- option `--pkg-name`
- option `--registry`
- option `--registry-scope`
- option `--registry-auth-token`
- flag `--no-bypass-cache`
- flag `--use-fetchgit-private`
- flag `--strip-optional-dependencies`
