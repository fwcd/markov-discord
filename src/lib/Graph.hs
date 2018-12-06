module Graph(
    GraphEdge,
    Graph (..),
    resolveEdge
) where

import ContainerUtils
import Data.List

type GraphEdge = (Int, Int)
data Graph a = Graph {
    graphNodes :: [a],
    graphEdges :: [GraphEdge]
}

resolveEdge :: GraphEdge -> Graph a -> (Maybe a, Maybe a)
resolveEdge e g = mapTuple (\n -> nth n $ graphNodes g) e

withNode :: (Eq a) => a -> Graph a -> Graph a
withNode x g
    | elem x $ graphNodes g = Graph {
        graphNodes = x : graphNodes g,
        graphEdges = mapTuple (+1) <$> graphEdges g
    }
    | otherwise = g

withEdge :: GraphEdge -> Graph a -> Graph a
withEdge x g
    | elem x $ graphEdges g = Graph {
        graphNodes = graphNodes g,
        graphEdges = x : graphEdges g
    }
    | otherwise = g

connect :: (Eq a) => a -> a -> Graph a -> Graph a
connect x y g = withNode x $ withNode y $
    withEdge (mapTuple (\v -> unwrap $ elemIndex v $ graphNodes g) (x, y)) g
