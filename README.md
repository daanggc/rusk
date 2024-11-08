<h1 align="center">
<img height="90" src="assets/rusk_logo_light.svg#gh-dark-mode-only" alt="Rusk">
<img height="90" src="assets/rusk_logo_dark.svg#gh-light-mode-only" alt="Rusk">
</h1>

<p align="center">
  The official <img height="11" src="assets/dusk_circular_light.svg#gh-dark-mode-only"><img height="11" src="assets/dusk_circular_dark.svg#gh-light-mode-only"><a href="https://dusk.network/"> Dusk</a> protocol node client and smart contract platform.
</p>

<p align=center>
<a href="https://github.com/dusk-network/rusk/actions/workflows/rusk_ci.yml">
<img src="https://github.com/dusk-network/rusk/actions/workflows/rusk_ci.yml/badge.svg" alt="Rusk CI"></a>
&nbsp;
<a href="https://github.com/dusk-network/rusk/actions/workflows/explorer_ci.yml">
<img src="https://github.com/dusk-network/rusk/actions/workflows/explorer_ci.yml/badge.svg" alt="explorer CI"></a>
&nbsp;
<a href="https://github.com/dusk-network/rusk/actions/workflows/webwallet_ci.yml">
<img src="https://github.com/dusk-network/rusk/actions/workflows/webwallet_ci.yml/badge.svg" alt="web-wallet CI"></a>

more badges:
- discord chat online
- CTA to star repo
- built with rust
- license mpl
</p>


<p align="center">
	<a href="https://dusk.network/news" target="_blank"><img height="25" src="assets/dusk_circular_light.svg" alt="News"></a><img height="11" src="assets/dusk_circular_dark.svg#gh-light-mode-only">
	&nbsp;
	<a href="https://github.com/dusk-network" target="_blank"><img height="25" src="assets/icons/github_mark.svg" alt="Github	"></a>
	&nbsp;
    <a href="https://www.linkedin.com/company/dusknetwork" target="_blank"><img height="25" src="assets/icons/linkedin.svg" alt="LinkedIn"></a>
    &nbsp;
    <a href="https://x.com/DuskFoundation" target="_blank"><img height="25" src="assets/icons/x_logo.svg" alt="X"></a>
    &nbsp;
    <a href="https://t.me/DuskNetwork" target="_blank"><img height=25 src="assets/icons/telegram.svg" alt="Telegram"></a>
    &nbsp;
    <a href="https://discord.gg/dusk-official" target="_blank"><img height="25" src="assets/icons/discord.svg" alt="Discord"></a>
</p>

> _Unstable_ : No guarantees can be made regarding the API stability, the project is in development.

# Table of Contents
- Run a node
  - Without docker
  - With docker
## How to run a node

For more information on running a node, see our docs: 
- [Node Setup](https://docs.dusk.network/getting-started/node-setup/overview)
- [Node Requirements](https://docs.dusk.network/getting-started/node-setup/node-requirements)

## Run a local node for development

### Prerequisites

- Rust 1.71 nightly or higher
- GCC 13 or higher
- Clang 16 or higher

### Rust Installation

Rusk makes use of the nightly toolchain, make sure it is installed. Furthermore, to build the WASM contracts, `wasm-pack` is required.

To install and set the nightly toolchain, and install `wasm-pack`, run:
```bash
rustup toolchain install nightly
rustup default nightly
cargo install wasm-pack
```

## Build and Tests

To build `rusk` from source, Rust, GCC and Clang are required. Once the dependencies are installed, you can simply run the following command to compile everything:

```bash
make
```

To run tests:

```bash
make test
```

That will also compile all the genesis contracts and its associated circuits. See also `make help` for all the available commands


## Run a local node for development

Run a single full-node cluster with example state.

### Prerequisites:

```bash
# Generate the keys used by the circuits
# Compile all the genesis contracts
# Copy example consensus.keys
make prepare-dev
```
### Node

```bash
# Launch a local ephemeral node
make run-dev
```
### Archive node

```bash
make run-dev-archive
```

### Prover Node

The node can be build as a prover only as follows:
```bash
cargo r --release --no-default-features --features prover -p rusk
```

This prover node will be accessible on `https://localhost:8080`. Apps like the [rusk-wallet](https://github.com/dusk-network/rusk/tree/master/rusk-wallet) can be connected to it for quicker and more private local proving.

## Contracts compilation

Compile all the genesis contracts without running the server:

```bash
make contracts
```
Compile a specific genesis contract:

```bash
# generate the wasm for `transfer` contract
make wasm for=transfer
```

## Docker support

It's also possible to run a local ephemeral node with Docker.

To build the Docker image:

```bash
docker build -t rusk .
```

To run Rusk inside a Docker container:

```bash
docker run -p 9000:9000/udp -p 8080:8080/tcp rusk
```

Port 9000 is used for Kadcast, port 8080 for the HTTP and GraphQL APIs.

## License

The Rusk software is licensed under the [Mozilla Public License Version 2.0](./LICENSE).
