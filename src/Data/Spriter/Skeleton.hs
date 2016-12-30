{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Spriter.Skeleton where

import Control.Lens
import Data.Graph
import Data.Scientific (toRealFloat)
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


data ResultBone = ResultBone
  { _rbAngle :: Double
  , _rbX :: Double
  , _rby :: Double
  } deriving (Eq, Show, Read)


animate :: [ObjInfo]
        -> Animation
        -> Int  -- ^ Frame.
        -> [ResultBone]
animate objs anim frame = fmap (uncurry lerpBones) tlKeys
  where
    keyframes = anim ^. animMainline.mainlineKey
    (kf1, kf2) = head . filter betweenKeyframes
                      . zip keyframes
                      $ tail keyframes
    tlKeys = zip (getTimelineKey <$> _mainlineKeyBoneRef kf1)
                 (getTimelineKey <$> _mainlineKeyBoneRef kf2)

    progress = normalize (fromIntegral $ _mainlineKeyTime kf1)
                         (fromIntegral $ _mainlineKeyTime kf2)
                         $ fromIntegral frame :: Double

    betweenKeyframes (k1, k2) = _mainlineKeyTime k1 >= frame
                             && _mainlineKeyTime k2 <  frame

    lerpBones tlk1 tlk2
      | Just b1 <- _timelineKeyBone tlk1
      , Just b2 <- _timelineKeyBone tlk2
      = let lerping f = lerp progress (toRealFloat $ f b1)
                                      (toRealFloat $ f b2)
         in ResultBone (lerping _timelineBoneAngle)
                       -- TODO(sandy): ^ use spin to figure this out
                       (lerping _timelineBoneX)
                       (lerping _timelineBoneY)

    getTimelineKey :: BoneRef -> TimelineKey
    getTimelineKey br =
      _timelineKey (_animTimeline anim !! _boneRefTimeline br)
        !! _boneRefKey br

normalize :: (Num a, Fractional a)
          => a  -- ^ Lower bound.
          -> a  -- ^ Upper bound.
          -> a  -- ^ Value.
          -> a  -- ^ Normalized value.
normalize l u v = (v - l) / (u - l)

lerp :: (Floating a) => a -> a -> a -> a
lerp p l u = l * (1 - p) + u * p

