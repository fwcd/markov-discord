module Graph(
    GraphNode,
    GraphEdge,
    Graph (..),
    resolveEdge
) where

import ContainerUtils

type GraphNode = String
type GraphEdge = (Int, Int)
data Graph = Graph {
    graphNodes :: [GraphNode],
    graphEdges :: [GraphEdge]
}

resolveEdge :: GraphEdge -> Graph -> (Maybe GraphNode, Maybe GraphNode)
resolveEdge e g = mapTuple (\n -> nth n $ graphNodes g) e
