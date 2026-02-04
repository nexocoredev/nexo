# Nexo

Nexo is a minimal, flow-first scripting language designed for scripting and automation workflows.
It favors pipelines and small functions to keep scripts readable and practical.

## Installation

### Windows
- ZIP: extract and add the folder to PATH
- Installer: run the .exe

### Linux
- Download the tar.gz
- `chmod +x nexo`
- Move to `/usr/local/bin` or add the folder to PATH

### macOS
- Download the tar.gz
- `chmod +x nexo`
- Add the folder to PATH
- If Gatekeeper blocks it: right-click > Open once

## First Script
```nexo
fs_exists("config.json") |> print
```

In Nexo, this is the program.

Run it with:

```bash
nexo run script.nx
```

## Status
- Version: v1.0
- License: MIT
- Platforms: Windows, macOS, Linux

## Package Layout
- `nexo.exe` / `nexo` — CLI binary
- `nexo_modules/` — standard modules
- `docs/` — official docs and starter script

## Included Modules
- env
- fs
- http
- log
- path
- proc
- py
- re
- time

## Links
- Website: https://nexocore.dev
- Docs: https://nexocore.dev/docs
