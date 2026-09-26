# npm Changelog

## Unreleased

### Added

- Run a standalone TypeScript React Flow example with orthogonal routes, measured labels, feedback edges, and adjustable spacing.

### Changed

- The React Flow example now measures content-sized nodes and automatically relayouts after resizing, with a guide to pixel/grid conversion and avoiding measurement loops.

### Fixed

- Captioned branches stay separated during post-routing compaction instead of collapsing nodes onto the same row.

## 0.0.1 - 2026-09-25

### Added

- Use the layered graph layout engine from JavaScript and TypeScript through `@markgrafhq/layered-layout`, with ESM/CommonJS bundles, ports, constraints, measured edge labels, and no runtime npm dependencies.
