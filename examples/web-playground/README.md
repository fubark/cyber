## Cyber Web Playground Example

This example shows how you can embed the Cyber interpreter into your webpage with an editor. It uses codemirror for the editor.

## Getting started.

Make sure you have built `cyber.wasm` in [Building](https://github.com/fubark/cyber/blob/master/docs/build.md) or downloaded from [Downloads](https://github.com/fubark/cyber/releases).

Copy cyber.wasm into this directory.

Start a local file server in this directory:
```sh
python3 -m http.server 8000
```

Open your browser and visit `http://localhost:8000/index.html`