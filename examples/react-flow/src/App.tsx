import { useMemo, useState } from "react";
import {
  Background,
  BaseEdge,
  Controls,
  EdgeLabelRenderer,
  Handle,
  MarkerType,
  Position,
  ReactFlow,
  type Edge,
  type EdgeProps,
  type Node,
  type NodeProps,
} from "@xyflow/react";
import {
  layout,
  scaleFactor,
  type EdgeLabel,
  type EdgePath,
  type Graph,
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

const services: Record<string, ServiceData> = {
  request: { title: "Incoming request", detail: "HTTP /api/orders" },
  validate: { title: "Validate", detail: "Schema + permissions" },
  cache: { title: "Cache", detail: "Redis" },
  worker: { title: "Order service", detail: "Business logic" },
  database: { title: "Database", detail: "PostgreSQL" },
  audit: { title: "Audit log", detail: "Event stream" },
  reply: { title: "Response", detail: "200 OK" },
};

const graph: Graph = {
  nodes: Object.keys(services).map(id => ({ id, size: [14, 5] })),
  edges: [
    { id: "request-validate", from: { node: "request" }, to: { node: "validate" } },
    { id: "validate-cache", from: { node: "validate" }, to: { node: "cache" } },
    { id: "validate-worker", from: { node: "validate" }, to: { node: "worker" } },
    { id: "worker-database", from: { node: "worker" }, to: { node: "database" } },
    { id: "worker-audit", from: { node: "worker" }, to: { node: "audit" } },
    { id: "database-reply", from: { node: "database" }, to: { node: "reply" } },
    { id: "cache-reply", from: { node: "cache" }, to: { node: "reply" }, label: "cache hit" },
  ],
};

function routePath(route: EdgePath): string {
  const first = route.segments[0];
  if (!first) return "";
  return `M ${first.start[0] * FINE_PX} ${first.start[1] * FINE_PX} ` +
    route.segments.map(segment => `L ${segment.end[0] * FINE_PX} ${segment.end[1] * FINE_PX}`).join(" ");
}

function computeLayout(feedback: boolean, nodeGap: number) {
  const input: Graph = {
    ...graph,
    edges: feedback
      ? [...graph.edges, { id: "retry", from: { node: "database" }, to: { node: "worker" }, label: "retry" }]
      : graph.edges,
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

  const nodes: ServiceNode[] = result.nodes.map(node => {
    const data = services[node.node];
    if (!data) throw new Error(`Unknown service ${node.node}`);
    return {
      id: node.node,
      type: "service",
      position: { x: node.position[0] * COARSE_PX, y: node.position[1] * COARSE_PX },
      style: { width: node.size[0] * COARSE_PX, height: node.size[1] * COARSE_PX },
      data,
    };
  });
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
  return { nodes, edges, milliseconds, metrics: result.metrics, feedbackEdges: result.edges.filter(edge => edge.reversed).length };
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

export function App() {
  const [feedback, setFeedback] = useState(false);
  const [nodeGap, setNodeGap] = useState(4);
  const computed = useMemo(() => computeLayout(feedback, nodeGap), [feedback, nodeGap]);

  return (
    <main>
      <header>
        <div>
          <p className="eyebrow">TYPESCRIPT CONSUMER EXAMPLE</p>
          <h1>Layered Layout <span>+ React Flow</span></h1>
          <p className="description">Markgraf computes the geometry. React Flow displays it.</p>
        </div>
        <a href="https://www.npmjs.com/package/@markgrafhq/layered-layout" target="_blank" rel="noreferrer">@markgrafhq/layered-layout <b>0.0.1</b></a>
      </header>
      <section className="toolbar" aria-label="Layout options">
        <label className="toggle"><input type="checkbox" checked={feedback} onChange={event => setFeedback(event.target.checked)} /> Add a feedback edge</label>
        <label className="spacing">Node gap <input type="range" min="2" max="10" step="1" value={nodeGap} onChange={event => setNodeGap(Number(event.target.value))} /><output>{nodeGap}</output></label>
        <output className="metrics" aria-label="Layout metrics">
          {computed.nodes.length} nodes · {computed.edges.length} edges · {computed.feedbackEdges} reversed · {computed.milliseconds.toFixed(1)} ms
        </output>
      </section>
      <div className="diagram" aria-label="Layout diagram">
        <ReactFlow<ServiceNode, RouteEdge>
          key={`${feedback}-${nodeGap}`}
          nodes={computed.nodes}
          edges={computed.edges}
          nodeTypes={nodeTypes}
          edgeTypes={edgeTypes}
          nodesDraggable={false}
          nodesConnectable={false}
          elementsSelectable={false}
          fitView
          fitViewOptions={{ padding: 0.15, maxZoom: 1.2 }}
          minZoom={0.25}
          maxZoom={2}
          colorMode="dark"
        >
          <Background gap={20} size={1} color="#29313e" />
          <Controls showInteractive={false} />
        </ReactFlow>
      </div>
      <footer>
        <span>Pan and zoom to explore. Positions are fixed to preserve the engine’s routes.</span>
        <span>{computed.metrics.bendCount} bends · {computed.metrics.nodeOverlapCount} overlaps · dashed edges indicate cycle removal</span>
      </footer>
    </main>
  );
}
