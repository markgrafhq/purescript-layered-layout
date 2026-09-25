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

### Measuring content-sized nodes

The layout engine does not render React components or measure text. Supply the
size of each **rendered node box**, including padding and borders—not just the
text width.

For React Flow, use this sequence:

1. Render the nodes with content-driven CSS and no fixed `width`/`height`.
   The example uses `width: max-content; max-width: 220px`, so long content wraps.
2. Inside a `ReactFlowProvider`, wait for
   [`useNodesInitialized()`](https://reactflow.dev/api-reference/hooks/use-nodes-initialized).
   React Flow's resize observer populates
   [`node.measured.width` and `node.measured.height`](https://reactflow.dev/api-reference/types/node)
   in unzoomed CSS pixels.
3. Convert those dimensions to coarse units and call `layout()`. Missing
   measurements mean “wait”, not “substitute a default rectangle”:

```ts
import { layout, type Graph, type LayoutResult } from "@markgrafhq/layered-layout";
import type { Node as FlowNode } from "@xyflow/react";

// An application-chosen scale, not a requirement of the engine.
const COARSE_PX = 12;

function layoutMeasuredNodes(
  nodes: readonly FlowNode[],
  edges: Graph["edges"],
): LayoutResult | null {
  const inputNodes: Array<Graph["nodes"][number]> = [];
  for (const node of nodes) {
    const { width, height } = node.measured ?? {};
    if (width === undefined || height === undefined || width <= 0 || height <= 0) {
      return null;
    }
    inputNodes.push({
      id: node.id,
      size: [width / COARSE_PX, height / COARSE_PX],
    });
  }
  return layout({ nodes: inputNodes, edges });
}
```

4. Multiply returned node positions by `COARSE_PX` and route/label coordinates
   by `COARSE_PX / scaleFactor`. Update positions and routes, but preserve node
   data and measurements. **Do not write layout sizes back into the
   content-driven CSS**; that can prevent shrinking or cause a resize loop.
5. Rerun when measured dimensions, graph connections, or layout options change.
   `useNodesInitialized()` gates initial readiness; it is not a subscription to
   every subsequent resize. The example uses `useStore` with an equality
   function comparing only node IDs and measured dimensions, so changing
   positions does not trigger layout again. Content edits and font/CSS changes
   that resize nodes do.

Keep fractional coarse sizes: a measured width of 135 px becomes `11.25`
coarse units at this scale. Do not round every node to a whole grid cell.
Do not multiply by `devicePixelRatio` or use a zoomed `getBoundingClientRect()`
as the layout size.

To hide the initial pile of unpositioned nodes, use `opacity: 0` and reveal them
after layout. `display: none` and React Flow's `hidden` flag prevent the
measurement needed to proceed. For custom renderers, a `ResizeObserver`
border-box measurement provides the corresponding DOM measurement; for web
fonts, wait for `document.fonts.ready` or rerun when loading changes dimensions.

Node measurement does not measure edge labels. Measure each dynamic label's
box with its actual font, wrapping, padding, and borders, then pass
`size: [labelWidthPx / COARSE_PX, labelHeightPx / COARSE_PX]` in `edgeLabels`.
The example's two edge labels have explicitly sized boxes.

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
The example measures content-sized nodes, converts both coordinate systems to
pixels, renders the engine's orthogonal routes through a custom React Flow
edge, and displays measured edge labels. Toggle **Longer node content** to see
the service node grow, wrap, and automatically reroute its edges; toggle it off
to shrink again. The feedback-edge and spacing controls also recompute layout.
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
