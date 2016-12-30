{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Spriter.Skeleton where

import Debug.Trace (trace)
import Data.String.Conv (toS)
import Data.Aeson (Result (..), decode, fromJSON)
import Control.Lens
import Data.Monoid ((<>))
import Data.Scientific (toRealFloat)
import Data.Spriter.Types


data ResultBone = ResultBone
  { _rbAngle :: Double
  , _rbX :: Double
  , _rby :: Double
  , _rbParent :: Maybe Int
  } deriving (Eq, Show, Read)
makeLenses ''ResultBone

instance Monoid ResultBone where
  mempty  = ResultBone 0 0 0 Nothing
  mappend (ResultBone a  x  y  _) (ResultBone a' x' y' p) =
    let s = sin a
        c = cos a
        x'' = (x' * c) - (y' * s)
        y'' = (x' * s) + (y' * c)
     in ResultBone (a + a') (x + x'') (y + y'') p


showTrace :: Show a => a -> a
showTrace = trace =<< show

animate :: Animation
        -> Int  -- ^ Frame.
        -> [ResultBone]
animate anim frame = result
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

    betweenKeyframes (k1, k2) =
      let t1 = _mainlineKeyTime k1
          t2 = _mainlineKeyTime k2
       in (  t1 <= frame
          && t2 >  frame
          ) || ( t1 > t2 && t2 > frame )

    result = accumulate <$> fmap (uncurry lerpBones) tlKeys
      where
        accumulate rb =
          case _rbParent rb of
            Just i  -> result !! i <> rb
            Nothing -> rb

    lerpBones (tlk1, parent) (tlk2, _)
      | Just b1 <- _timelineKeyBone tlk1
      , Just b2 <- _timelineKeyBone tlk2
      = let lerping f = lerp progress (toRealFloat $ f b1)
                                      (toRealFloat $ f b2)
         in ResultBone (degToRad $ lerping _timelineBoneAngle)
                       -- TODO(sandy): ^ use spin to figure this out
                       (lerping _timelineBoneX)
                       (lerping _timelineBoneY)
                       parent
    lerpBones _ _ = error "bad lerpbones"

    getTimelineKey :: BoneRef -> (TimelineKey, Maybe Int)
    getTimelineKey br =
      (, _boneRefParent br)
        $  _timelineKey (_animTimeline anim !! _boneRefTimeline br)
        !! _boneRefKey br


normalize :: (Fractional a)
          => a  -- ^ Lower bound.
          -> a  -- ^ Upper bound.
          -> a  -- ^ Value.
          -> a  -- ^ Normalized value.
normalize l u v = (v - l) / (u - l)

lerp :: (Floating a) => a -> a -> a -> a
lerp p l u = l * (1 - p) + u * p

degToRad :: (Floating a) => a -> a
degToRad a = a / 180 * pi



test :: IO ()
test = do
  file <- readFile "/home/bootstrap/Projects/bones/basic-anim.scon"
  let Just x = decode $ toS file
      Success y = fromJSON x
      animation = y ^. schemaEntity._head.entityAnimation
  print $ animate (head animation) 202

