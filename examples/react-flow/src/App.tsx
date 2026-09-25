import { useEffect, useState } from "react";
import {
  Background,
  BaseEdge,
  Controls,
  EdgeLabelRenderer,
  Handle,
  MarkerType,
  Position,
  ReactFlow,
  ReactFlowProvider,
  useNodesInitialized,
  useReactFlow,
  useStore,
  type Edge,
  type EdgeProps,
  type Node,
  type NodeProps,
  type ReactFlowState,
} from "@xyflow/react";
import {
  layout,
  scaleFactor,
  type EdgeLabel,
  type EdgePath,
  type Graph,
  type Metrics,
} from "@markgrafhq/layered-layout";

// React Flow works in pixels. The engine returns coarse node geometry and fine
// edge/label geometry, so both must be converted to the same pixel coordinates.
const COARSE_PX = 12;
const FINE_PX = COARSE_PX / scaleFactor;

type ServiceData = { title: string; detail: string };
type ServiceNode = Node<ServiceData, "service">;
type RouteEdge = Edge<{
  path: string;
  label?: { text: string; x: number; y: number; width: number; height: number };
}, "routed">;

interface ComputedLayout {
  positions: Map<string, { x: number; y: number }>;
  nodeCount: number;
  edges: RouteEdge[];
  milliseconds: number;
  metrics: Metrics;
  feedbackEdges: number;
}

const services = {
  request: { title: "Incoming request", detail: "HTTP /api/orders" },
  validate: { title: "Validate", detail: "Schema + permissions" },
  cache: { title: "Cache", detail: "Redis" },
  worker: { title: "Order service", detail: "Business logic" },
  database: { title: "Database", detail: "PostgreSQL" },
  audit: { title: "Audit log", detail: "Event stream" },
  reply: { title: "Response", detail: "200 OK" },
} satisfies Record<string, ServiceData>;

// Opacity keeps the first, overlapping render invisible without preventing
// React Flow's ResizeObserver from measuring each node's real border box.
const initialNodes: ServiceNode[] = Object.entries(services).map(([id, data]) => ({
  id,
  type: "service",
  position: { x: 0, y: 0 },
  data,
  style: { opacity: 0 },
}));
const initialEdges: RouteEdge[] = [];

const graphEdges: Graph["edges"] = [
  { id: "request-validate", from: { node: "request" }, to: { node: "validate" } },
  { id: "validate-cache", from: { node: "validate" }, to: { node: "cache" } },
  { id: "validate-worker", from: { node: "validate" }, to: { node: "worker" } },
  { id: "worker-database", from: { node: "worker" }, to: { node: "database" } },
  { id: "worker-audit", from: { node: "worker" }, to: { node: "audit" } },
  { id: "database-reply", from: { node: "database" }, to: { node: "reply" } },
  { id: "cache-reply", from: { node: "cache" }, to: { node: "reply" }, label: "cache hit" },
];

function routePath(route: EdgePath): string {
  const first = route.segments[0];
  if (!first) return "";
  return `M ${first.start[0] * FINE_PX} ${first.start[1] * FINE_PX} ` +
    route.segments.map(segment => `L ${segment.end[0] * FINE_PX} ${segment.end[1] * FINE_PX}`).join(" ");
}

function computeLayout(measuredNodes: readonly Node[], feedback: boolean, nodeGap: number): ComputedLayout | null {
  const inputNodes: Array<Graph["nodes"][number]> = [];
  for (const node of measuredNodes) {
    const { width, height } = node.measured ?? {};
    // Never substitute a guessed size while a node is still being measured.
    if (width === undefined || height === undefined || width <= 0 || height <= 0) return null;
    inputNodes.push({ id: node.id, size: [width / COARSE_PX, height / COARSE_PX] });
  }
  const input: Graph = {
    nodes: inputNodes,
    edges: feedback
      ? [...graphEdges, { id: "retry", from: { node: "database" }, to: { node: "worker" }, label: "retry" }]
      : graphEdges,
  };
  const edgeLabels: Record<string, EdgeLabel> = {
    "cache-reply": { size: [6, 2], placement: "Center" },
  };
  if (feedback) edgeLabels.retry = { size: [4, 2], placement: "Tail" };

  const start = performance.now();
  const result = layout(input, { nodeGap, edgeLabels });
  const milliseconds = performance.now() - start;
  const connections = new Map(input.edges.map(edge => [edge.id, edge]));
  const labels = new Map(result.edgeLabels.map(label => [label.edge, label]));

  const positions = new Map(result.nodes.map(node => [
    node.node,
    { x: node.position[0] * COARSE_PX, y: node.position[1] * COARSE_PX },
  ]));
  const edges: RouteEdge[] = result.edges.map(route => {
    const connection = connections.get(route.edge);
    if (!connection) throw new Error(`Unknown edge ${route.edge}`);
    const label = labels.get(route.edge);
    const color = route.reversed ? "#d49bff" : "#7192aa";
    return {
      id: route.edge,
      source: connection.from.node,
      target: connection.to.node,
      type: "routed",
      markerEnd: { type: MarkerType.ArrowClosed, color, width: 16, height: 16 },
      style: { stroke: color, strokeWidth: 1.5, strokeDasharray: route.reversed ? "5 4" : undefined },
      data: {
        path: routePath(route),
        label: label && connection.label ? {
          text: connection.label,
          x: label.position[0] * FINE_PX,
          y: label.position[1] * FINE_PX,
          width: label.size[0] * FINE_PX,
          height: label.size[1] * FINE_PX,
        } : undefined,
      },
    };
  });
  return { positions, nodeCount: result.nodes.length, edges, milliseconds, metrics: result.metrics, feedbackEdges: result.edges.filter(edge => edge.reversed).length };
}

function Service({ data }: NodeProps<ServiceNode>) {
  return (
    <div className="service-node">
      <Handle type="target" position={Position.Top} />
      <strong>{data.title}</strong>
      <span>{data.detail}</span>
      <Handle type="source" position={Position.Bottom} />
    </div>
  );
}

function RoutedEdge({ id, data, markerEnd, style }: EdgeProps<RouteEdge>) {
  if (!data) throw new Error(`Missing route data for ${id}`);
  return (
    <>
      <BaseEdge id={id} path={data.path} markerEnd={markerEnd} style={style} />
      {data.label && (
        <EdgeLabelRenderer>
          <div className="route-label" style={{
            transform: `translate(${data.label.x}px, ${data.label.y}px)`,
            width: data.label.width,
            height: data.label.height,
          }}>{data.label.text}</div>
        </EdgeLabelRenderer>
      )}
    </>
  );
}

const nodeTypes = { service: Service };
const edgeTypes = { routed: RoutedEdge };

// Moving nodes must not trigger another layout. Subscribe only to IDs and
// measured dimensions, including later changes from content, CSS, or fonts.
function sameNodeSizes(previous: Node[], next: Node[]): boolean {
  return previous.length === next.length && previous.every((node, index) => {
    const other = next[index];
    return other !== undefined && node.id === other.id &&
      node.measured?.width === other.measured?.width &&
      node.measured?.height === other.measured?.height;
  });
}

function Diagram() {
  const [feedback, setFeedback] = useState(false);
  const [nodeGap, setNodeGap] = useState(4);
  const [expanded, setExpanded] = useState(false);
  const [computed, setComputed] = useState<ComputedLayout | null>(null);
  const nodesInitialized = useNodesInitialized();
  const measuredNodes = useStore(
    (state: ReactFlowState) => state.nodes,
    sameNodeSizes,
  );
  const { setNodes, setEdges, fitView } = useReactFlow<ServiceNode, RouteEdge>();

  useEffect(() => {
    if (!nodesInitialized) return;
    const next = computeLayout(measuredNodes, feedback, nodeGap);
    if (!next) return;

    // Preserve the current data, measured sizes, and authored node order.
    // Only positions change: writing sizes back into CSS can create a loop.
    setNodes(current => current.map(node => {
      const position = next.positions.get(node.id);
      return position ? { ...node, position, style: { ...node.style, opacity: 1 } } : node;
    }));
    setEdges(next.edges);
    setComputed(next);
    const frame = requestAnimationFrame(() => {
      void fitView({ padding: 0.15, maxZoom: 1.2 });
    });
    return () => cancelAnimationFrame(frame);
  }, [nodesInitialized, measuredNodes, feedback, nodeGap, setNodes, setEdges, fitView]);

  return (
    <main>
      <header>
        <div>
          <p className="eyebrow">TYPESCRIPT CONSUMER EXAMPLE</p>
          <h1>Layered Layout <span>+ React Flow</span></h1>
          <p className="description">React Flow measures the content. Markgraf lays out the measured boxes.</p>
        </div>
        <a href="https://www.npmjs.com/package/@markgrafhq/layered-layout" target="_blank" rel="noreferrer">@markgrafhq/layered-layout <b>0.0.1</b></a>
      </header>
      <section className="toolbar" aria-label="Layout options">
        <label className="toggle"><input type="checkbox" checked={feedback} onChange={event => setFeedback(event.target.checked)} /> Add a feedback edge</label>
        <label className="toggle"><input type="checkbox" checked={expanded} onChange={event => {
          const checked = event.target.checked;
          setExpanded(checked);
          setNodes(current => current.map(node => node.id === "worker" ? {
            ...node,
            data: { ...node.data, detail: checked ? "Validate inventory, calculate shipping, and reserve the requested items before checkout." : services.worker.detail },
          } : node));
        }} /> Longer node content</label>
        <label className="spacing">Node gap <input type="range" min="2" max="10" step="1" value={nodeGap} onChange={event => setNodeGap(Number(event.target.value))} /><output>{nodeGap}</output></label>
        <output className="metrics" aria-label="Layout metrics" aria-live="polite">
          {computed ? `${computed.nodeCount} measured nodes · ${computed.edges.length} edges · ${computed.feedbackEdges} reversed · ${computed.milliseconds.toFixed(1)} ms` : "Measuring nodes…"}
        </output>
      </section>
      <div className="diagram" aria-label="Layout diagram">
        <ReactFlow<ServiceNode, RouteEdge>
          defaultNodes={initialNodes}
          defaultEdges={initialEdges}
          nodeTypes={nodeTypes}
          edgeTypes={edgeTypes}
          nodesDraggable={false}
          nodesConnectable={false}
          elementsSelectable={false}
          minZoom={0.25}
          maxZoom={2}
          colorMode="dark"
        >
          <Background gap={20} size={1} color="#29313e" />
          <Controls showInteractive={false} />
        </ReactFlow>
      </div>
      <footer>
        <span>Content-sized nodes. Pan and zoom; positions stay fixed to preserve routes.</span>
        <span>{computed ? `${computed.metrics.bendCount} bends · ${computed.metrics.nodeOverlapCount} overlaps · dashed edges indicate cycle removal` : "Waiting for node measurements"}</span>
      </footer>
    </main>
  );
}

export function App() {
  return <ReactFlowProvider><Diagram /></ReactFlowProvider>;
}
