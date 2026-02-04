# Hello Real World (Nexo 1.0)
# Hello Real World (Nexo 1.0)

This is the recommended first script: a real, runnable flow that fetches data, transforms it, and logs the result.
It is included as `docs/hello_real_world.nx`.

Run it with:

```
nexo run docs/hello_real_world.nx
```

```nexo
import log
import re

def extract_title(res):
    titles = re.findall("<title>[^<]+</title>", res["body"])
    if len(titles) == 0:
        return "<no title>"
    return re.replace("<[^>]+>", titles[0], "")

"https://example.com"
    |> http_get
    |> extract_title
    |> log.info
    |> print
```

Why this is the starting point:
- It uses real I/O (`http_get`).
- It keeps a flow-first mental model with `|>`.
- It shows logging without breaking the pipeline.
