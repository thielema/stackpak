{-# LANGUAGE DuplicateRecordFields #-}
module DependencyResolver where

import Data.Graph
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
import Data.List
import Data.Maybe
import Data.Text (Text)

import Stack.LtsYaml
import Util

-- |Given a List of Packages, return them in the required build order based on their dependencies.
-- |TODO: clean up code.
resolve :: [Package] -> [Package]
resolve pkgs = result
    where
        pkgDeps :: Package -> [Text]
        pkgDeps pkg = map (name :: DependencyPackage -> Text) (getPackageDependencies pkg)

        -- create a HashMap for (package name, [dependency package name]).
        hashMap = HMS.fromList $ map (\pkg -> ((name :: Package -> Text) pkg, pkgDeps pkg)) pkgs

        -- lookup table for vertex -> key
        hmLookup = HMS.fromList $ zip [1..] (HMS.keys hashMap) :: HMS.HashMap Int Text
        -- lookup table for key -> vertex
        hmLookupRev = HMS.fromList $ concatMap (\k -> maybeTupleToList (HMS.lookup k hmLookup, Just k)) (HMS.keys hmLookup)
        -- root of the graph is 0
        graphRoot = 0 :: Int
        -- root edges
        rootEdges = zip (repeat graphRoot) (HMS.keys hmLookup)

        depsToVertices :: [Text] -> [Int]
        depsToVertices deps = mapMaybe (\depName -> HMS.lookup depName hmLookupRev) deps

        pkgToEdges :: (Text, [Text]) -> [(Int, Int)]
        pkgToEdges (pkgName, deps) = case HMS.lookup pkgName hmLookupRev of
            Just vertex -> zip (depsToVertices deps) (repeat vertex)
            Nothing -> []
        
        bounds = (graphRoot, maximum $ HMS.keys hmLookup)
        edges = rootEdges ++ concatMap pkgToEdges (HMS.toList hashMap)

        graph = buildG bounds edges
        vertices = topSort graph

        -- names of the packages in build order.
        orderedPkgNames = mapMaybe (\k -> HMS.lookup k hmLookup) vertices

        -- list of packages in build order.
        result = concatMap (\pkgName -> filter (\pkg -> (name :: Package -> Text) pkg == pkgName) pkgs) orderedPkgNames 
