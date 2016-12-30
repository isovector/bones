{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Spriter.Skeleton where

import Control.Lens
import Data.Graph
import Data.Maybe (maybeToList)
import Data.Spriter.Types

type BoneId = Int

data Skeleton = Skeleton
    { skeleton :: Graph
    , getBone' :: Vertex -> ((BoneRef, ObjInfo), BoneId, [BoneId])
    , getVertex :: BoneId -> Maybe Vertex
    }

getBone :: Skeleton -> BoneId -> Maybe (BoneRef, ObjInfo)
getBone r cid = do
    v <- getVertex r cid
    return . view _1 $ getBone' r v

getBoneId :: Skeleton -> Vertex -> BoneId
getBoneId r = view _2 . getBone' r


-- TODO(sandy): Assumes the root is bone with id 0.
makeSkeleton :: [ObjInfo] -> [BoneRef] -> Skeleton
makeSkeleton bones refs =
    let (  transposeG -> skeleton
         , getBone'
         , getVertex
         ) = graphFromEdges $ fmap toEdge refs
     in Skeleton {..}
  where
    toEdge c = ( (c, bones !! _boneRefId c)
               , _boneRefId c
               , maybeToList $ _boneRefParent c
               )

