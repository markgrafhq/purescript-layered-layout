/** A coordinate pair [x, y]. See each field's coarse/fine unit annotation. */
export type Position = readonly [x: number, y: number];
export type Size = readonly [width: number, height: number];
export type Side = "North" | "South" | "East" | "West";
export type Shape =
  | "Rectangle"
  | "Cylinder"
  | "Parallelogram"
  | "Diamond"
  | "Ellipse"
  | "Document"
  | "Cloud";

export interface Port {
  readonly id: string;
  readonly side: Side;
  /** Offset along the side, in coarse units (integer). */
  readonly offset: number;
  readonly label?: string;
}

export interface Node {
  readonly id: string;
  /** Physical body size in coarse units. */
  readonly size: Size;
  readonly ports?: readonly Port[];
  readonly label?: string;
  readonly shape?: Shape;
}

export interface Endpoint {
  readonly node: string;
  readonly port?: string;
}

export interface Edge {
  readonly id: string;
  readonly from: Endpoint;
  readonly to: Endpoint;
  readonly label?: string;
}

export type LayerPin =
  | { readonly type: "FirstLayer" }
  | { readonly type: "LastLayer" }
  | { readonly type: "SpecificLayer"; readonly value: number };

export type Constraint =
  | {
      readonly type: "AlignGroup";
      readonly value: {
        readonly nodes: readonly string[];
        readonly axis: "Horizontal" | "Vertical";
        readonly alignment: "Start" | "Center" | "End";
        readonly justify:
          | "JustifyStart"
          | "JustifyEnd"
          | "JustifyCenter"
          | "SpaceBetween"
          | "SpaceAround";
      };
    }
  | { readonly type: "SameLayer"; readonly value: { readonly nodes: readonly string[] } }
  | { readonly type: "LayerConstraint"; readonly value: { readonly node: string; readonly pin: LayerPin } }
  | { readonly type: "OrderConstraint"; readonly value: { readonly before: string; readonly after: string } }
  | {
      readonly type: "RelativePosition";
      readonly value: {
        readonly anchor: string;
        readonly target: string;
        /** Relative offset in coarse units. */
        readonly offset: Position;
      };
    };

export interface Graph {
  readonly nodes: readonly Node[];
  readonly edges: readonly Edge[];
  readonly constraints?: readonly Constraint[];
}

export interface EdgeLabel {
  /** Measured size in coarse units; label text is not measured by the engine. */
  readonly size: Size;
  readonly placement?: "Center" | "Tail" | "TailCenterTerminalRun";
}

/** Painted outsets in fine units; do not change the physical node size. */
export interface Margins {
  readonly left: number;
  readonly right: number;
  readonly top: number;
  readonly bottom: number;
}

export interface LayoutOptions {
  /** Within-layer spacing in coarse units (integer). */
  readonly nodeGap?: number;
  /** Between-layer spacing in coarse units (integer). */
  readonly layerGap?: number;
  readonly iterations?: number;
  readonly maxGapCount?: number;
  readonly layerer?: "NetworkSimplex" | "LongestPath";
  readonly cycleBreaker?: "Greedy" | "DepthFirst";
  readonly compactPostRouting?: boolean;
  /** Post-routing clearances in fine units. */
  readonly compactionSpacings?: {
    readonly nodeNode: number;
    readonly edgeNode: number;
    readonly edgeEdge: number;
  };
  readonly edgeLabels?: Readonly<Record<string, EdgeLabel>>;
  readonly nodeVisualMargins?: Readonly<Record<string, Margins>>;
}

export interface NodePlacement {
  readonly node: string;
  /** Top-left position in coarse units. */
  readonly position: Position;
  /** Physical body size in coarse units. */
  readonly size: Size;
  readonly layer: number;
  readonly order: number;
}

export interface EdgeSegment {
  /** Fine units. */
  readonly start: Position;
  /** Fine units. */
  readonly end: Position;
  readonly direction: "H" | "V";
}

export interface EdgePath {
  readonly edge: string;
  readonly segments: readonly EdgeSegment[];
  /** Fine units. */
  readonly bends: readonly Position[];
  readonly bendType: readonly ("LeftTurn" | "RightTurn")[];
  readonly jumps: readonly { readonly position: Position; readonly crossingEdge: string }[];
  /** Cycle removal reversed this edge internally; segments retain authored from-to order. */
  readonly reversed: boolean;
}

export interface EdgeLabelPlacement {
  readonly edge: string;
  /** Fine units. */
  readonly position: Position;
  /** Fine units. */
  readonly size: Size;
}

export interface Metrics {
  readonly crossingCount: number;
  readonly bendCount: number;
  /** Fine units. */
  readonly totalEdgeLength: number;
  /** Fine units. */
  readonly maxEdgeLength: number;
  readonly nodeOverlapCount: number;
  readonly constraintViolations: number;
  readonly jumpCount: number;
}

export interface LayoutResult {
  readonly nodes: readonly NodePlacement[];
  readonly edges: readonly EdgePath[];
  readonly edgeLabels: readonly EdgeLabelPlacement[];
  /** Bounds in coarse units, including routes, labels, and painted outsets. */
  readonly boundingBox: { readonly pos: Position; readonly size: Size };
  readonly metrics: Metrics;
}

/** Fine units per coarse unit. Multiply node/bounds coordinates by this when rendering routes. */
export declare const scaleFactor: number;

/**
 * Compute a deterministic top-to-bottom layout synchronously, without mutating inputs.
 * Throws Error for undecodable fields or unknown strategy/label-placement values.
 * IDs must be unique and references must identify existing nodes/ports.
 */
export declare function layout(graph: Graph, options?: LayoutOptions): LayoutResult;
