# purescript-layered-layout

PureScript layered-graph layout engine based on ELK algorithms.

Includes cycle removal, layer assignment, crossing minimization, coordinate assignment, port distribution, orthogonal routing, hyperedge routing, network-simplex compaction, and layout quality metrics.

## Build

```sh
spago build
```

The public entry point is `LayeredLayout.layout`.

## Measured edge labels

`Config.edgeLabels` maps each `EdgeId` to an `EdgeLabelSpec` pairing a coarse-unit `GridSize` with a placement:

- `Center` uses the existing ELK center-label dummy and routing pipeline.
- `Tail Adjacent` reserves a label beside the authored source, including feedback edges reversed during cycle removal.
- `Tail CenterTerminalRun` centers that source label between the painted source boundary and the first bend when clearance permits. Straight or blocked runs retain the source-side reservation.

Tail cells are reserved before coordinate assignment and remain routing obstacles through compaction. The final layout result owns their coordinates; consumers should not reposition them afterward.

`Config.nodeVisualMargins` supplies renderer-neutral painted outsets without enlarging physical node bodies or moving their ports. These margins, returned routes, and returned label coordinates use fine units; input node and label sizes use coarse units (one coarse unit is four fine units). Changing label size, placement, alignment, or visual margins invalidates cached layout phases.
