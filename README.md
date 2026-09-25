# Layered Layout

PureScript layered-graph layout engine based on ELK algorithms.

Includes cycle removal, layer assignment, crossing minimization, coordinate assignment, port distribution, orthogonal routing, hyperedge routing, network-simplex compaction, and layout quality metrics.

## JavaScript and TypeScript

```sh
npm install @markgrafhq/layered-layout
```

```ts
import { layout, scaleFactor, type Graph } from "@markgrafhq/layered-layout";

const graph: Graph = {
  nodes: [
    { id: "source", size: [8, 4] },
    { id: "target", size: [6, 3] },
  ],
  edges: [
    { id: "connection", from: { node: "source" }, to: { node: "target" } },
  ],
};

const result = layout(graph);
// Node rectangles and boundingBox use coarse units.
// Edge routes and output label rectangles use fine units.
const rectangles = result.nodes.map(node => ({
  id: node.node,
  x: node.position[0] * scaleFactor,
  y: node.position[1] * scaleFactor,
  width: node.size[0] * scaleFactor,
  height: node.size[1] * scaleFactor,
}));
console.log(rectangles, result.edges, result.boundingBox);
```

The package bundles the engine and its dependencies: no PureScript compiler,
runtime npm dependencies, DOM, or renderer is required. It provides ESM imports,
CommonJS `require("@markgrafhq/layered-layout")`, and declarations for both.

### API

`layout(graph, options?)` computes a deterministic, synchronous, top-to-bottom
layout without mutating either argument. It returns node placements, orthogonal
edge segments, bends, line jumps, measured label placements, a bounding box, and
layout quality metrics. It does not render or measure text. Large layouts run on
the calling thread; browser applications can call the same API in their own worker.

Nodes require `id` and `size: [width, height]`. Their `ports`, `label`, and `shape`
are optional; the default shape is `"Rectangle"`. Edges require `id`, `from`, and
`to`, where endpoints have `{ node, port? }`. Graph constraints are optional.
IDs must be unique within their kind, and endpoint/constraint references must
identify existing nodes and ports. Dimensions and spacing must be finite and
nonnegative; iteration counts must be positive integers. The boundary rejects
undecodable fields and unknown strategy or label-placement values with a
JavaScript `Error`; it does not independently validate graph topology.

The optional second argument exposes the engine's configuration:

- `nodeGap`, `layerGap`, `iterations`, `maxGapCount`.
- `layerer`: `"NetworkSimplex"` or `"LongestPath"`.
- `cycleBreaker`: `"Greedy"` or `"DepthFirst"`.
- `compactPostRouting` and `compactionSpacings: { nodeNode, edgeNode, edgeEdge }`.
- `edgeLabels`: an object keyed by edge ID, with measured `size` and optional
  `placement`: `"Center"` (default), `"Tail"`, or `"TailCenterTerminalRun"`.
- `nodeVisualMargins`: an object keyed by node ID, with `{ left, right, top, bottom }`.

```ts
const labeled = layout(graph, {
  edgeLabels: {
    connection: { size: [5, 2], placement: "Tail" },
  },
  nodeVisualMargins: {
    source: { left: 0, right: 0, top: 2, bottom: 0 },
  },
});
```

Ports have `{ id, side, offset, label? }`. Sides are `"North"`, `"South"`,
`"East"`, or `"West"`; offsets are integer coarse units along the side.
Constraints use the engine's tagged JSON representation, for example:

```ts
const constrained = layout({
  nodes: [{ id: "a", size: [4, 3] }, { id: "b", size: [4, 3] }],
  edges: [],
  constraints: [
    { type: "SameLayer", value: { nodes: ["a", "b"] } },
    { type: "OrderConstraint", value: { before: "a", after: "b" } },
  ],
});
```

The exported `Constraint` union also includes `AlignGroup`, `LayerConstraint`
(first, last, or specific layer), and `RelativePosition`. The declaration file
describes every input and result field. Cycles, self-loops, and disconnected
components are supported. Returned segment arrays follow the authored
source-to-target order even when `reversed` records internal cycle removal.

### Coordinate units

The npm API preserves the PureScript engine's units; `scaleFactor` is `4`.

| Geometry | Units |
| --- | --- |
| Input node/label sizes, port offsets, relative-position offsets | Coarse |
| `nodeGap`, `layerGap` | Coarse |
| Returned node positions/sizes and `boundingBox` | Coarse |
| Edge segment endpoints, bends, jumps, returned label positions/sizes | Fine |
| `compactionSpacings`, `nodeVisualMargins`, edge-length metrics | Fine |

Multiply coarse coordinates by `scaleFactor` to render everything in the same
space. Measured labels reserve layout space; setting an edge's text `label`
alone does not measure or reserve that space.

### React Flow example

[`examples/react-flow`](examples/react-flow) is a standalone React + TypeScript
app that installs the published npm package, without workspace aliases or
PureScript tooling:

```sh
cd examples/react-flow
npm ci
npm run dev
```

`npm run build` runs strict TypeScript checking and creates a production bundle.
The example converts both coordinate systems to pixels, renders the engine's
orthogonal routes through a custom React Flow edge, and displays measured edge
labels. Toggle the feedback edge or change node spacing to recompute the layout.
Pan and zoom are enabled; node dragging is disabled so it cannot detach the
engine-owned routes from their endpoints.

## Building from source

```sh
npm ci
npm run build
npm test
```

`npm run build` compiles PureScript and produces self-contained ESM/CommonJS
bundles, declarations, and dependency license notices in `dist/`. `npm pack`
rebuilds the package before creating a tarball. Releases must stay on `0.0.x`;
publish with `npm publish --access public` after checking the packed artifact.
The npm version in `package.json` is independent of the PureScript registry
version in `spago.yaml`.

PureScript consumers can continue to use `spago build`; their public entry
point is `LayeredLayout.layout`.

## Measured edge labels

`Config.edgeLabels` maps each `EdgeId` to an `EdgeLabelSpec` pairing a coarse-unit `GridSize` with a placement:

- `Center` uses the existing ELK center-label dummy and routing pipeline.
- `Tail Adjacent` reserves a label beside the authored source, including feedback edges reversed during cycle removal.
- `Tail CenterTerminalRun` centers that source label between the painted source boundary and the first bend when clearance permits. Straight or blocked runs retain the source-side reservation.

Tail cells are reserved before coordinate assignment and remain routing obstacles through compaction. The final layout result owns their coordinates; consumers should not reposition them afterward.

`Config.nodeVisualMargins` supplies renderer-neutral painted outsets without enlarging physical node bodies or moving their ports. These margins, returned routes, and returned label coordinates use fine units; input node and label sizes use coarse units (one coarse unit is four fine units). Changing label size, placement, alignment, or visual margins invalidates cached layout phases.

## License

Original code is MIT-licensed. ELK-derived files are EPL-2.0; their individual
copyright and upstream revision notices are retained. npm distributions include
the corresponding PureScript sources in `src/`, both license texts, and bundled
dependency notices in `dist/THIRD-PARTY-NOTICES.txt`.
