# Nexo Style Guide (1.0)
# Nexo Style Guide (1.0)

Nexo is flow-first. The pipeline operator `|>` is the unit of thought: values move forward through small, focused functions.

## Flow-First Philosophy
- Think in stages, not statements.
- Prefer pipelines over nested calls.
- Keep each stage simple and readable.

## Using `|>` Well
- Use `|>` when each step is a meaningful transform.
- Keep pipelines short and purposeful (3–6 stages is a good default).
- Name helper functions so they read like a flow.
- Prefer multiline pipelines when a single line becomes hard to scan.

## Comments
- Use `#` for single-line notes.
- Keep comments short and intent-focused.

## Small Functions
- Write small, single-purpose functions.
- Functions should return values that can continue a pipeline.
- Avoid hidden side effects; make effects explicit at the pipeline edges.

## Logging
- Use `log.info`, `log.warn`, `log.error` in pipelines to observe flow.
- Logging should not change values; it should pass them through.
- Prefer one log per stage when debugging, then remove or keep only essentials.

## What to Avoid
- Long pipelines with no clear meaning.
- Deep nesting when a pipeline can express the flow.
- Unnecessary side effects in the middle of a pipeline.
- Mixing formatting styles; rely on `nexo fmt` for canonical layout.
