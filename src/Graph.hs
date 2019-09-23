module Graph(
    GraphEdge,
    Graph (..),
    resolveEdge,
    emptyGraph,
    withNode,
    withEdge,
    connect,
    connectAll
) where

import ContainerUtils
import Data.List
import Data.Maybe

type GraphEdge = (Int, Int)
data Graph a = Graph {
    graphNodes :: [a],
    graphEdges :: [GraphEdge]
} deriving (Show)

resolveEdge :: GraphEdge -> Graph a -> (Maybe a, Maybe a)
resolveEdge e g = mapTuple (\n -> nth n $ graphNodes g) e

emptyGraph :: Graph a
emptyGraph = Graph { graphNodes = [], graphEdges = [] }

withNode :: (Eq a) => a -> Graph a -> Graph a
withNode x g
    | notElem x $ graphNodes g = Graph {
        graphNodes = x : graphNodes g,
        graphEdges = mapTuple (+1) <$> graphEdges g
    }
    | otherwise = g

withEdge :: GraphEdge -> Graph a -> Graph a
withEdge x g
    | notElem x $ graphEdges g = Graph {
        graphNodes = graphNodes g,
        graphEdges = x : graphEdges g
    }
    | otherwise = g

connect :: (Eq a) => a -> a -> Graph a -> Graph a
connect x y g = withEdge (mapTuple (\v -> fromJust $ elemIndex v $ graphNodes withNewNodes) (x, y)) withNewNodes
    where withNewNodes = withNode x $ withNode y g

connectAll :: (Eq a) => [(a, a)] -> Graph a -> Graph a
connectAll [] = id
connectAll ((x, y):xs) = connectAll xs . connect x y
