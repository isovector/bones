{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Spriter.Skeleton where

import Control.Lens
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Scientific (toRealFloat)
import Data.Spriter.Types


data ResultBone = ResultBone
  { _rbAngle  :: Double
  , _rbX      :: Double
  , _rbY      :: Double
  , _rbParent :: Maybe Int
  , _rbObj    :: Maybe BoneObj
  , _rbZIndex :: Maybe Int
  } deriving (Eq, Show, Read)
makeLenses ''ResultBone

isBone :: ResultBone -> Bool
isBone = not . isJust . _rbObj

instance Monoid ResultBone where
  mempty  = ResultBone 0 0 0 Nothing Nothing Nothing
  mappend (ResultBone a x y _ _ _) (ResultBone a' x' y' p o z) =
    let s = sin a
        c = cos a
        x'' = (x' * c) - (y' * s)
        y'' = (x' * s) + (y' * c)
     in ResultBone (a + a') (x + x'') (y + y'') p o z

animate :: Animation
        -> Double  -- ^ Frame.
        -> Maybe ([ResultBone])
animate anim frame =
  case frame <= anim ^. animLength of
    True  -> Just $ sortBy (comparing _rbZIndex) result
    False -> Nothing
  where
    keyframes = anim ^. animMainline.mainlineKey
    allKeyframes = (head keyframes : keyframes)
                ++ [head keyframes & mainlineKeyTime .~ _animLength anim]

    (kf1, kf2) = head . filter betweenKeyframes
                      . zip allKeyframes
                      $ tail allKeyframes
    tlKeys = zip (getTimelineKey <$> bonerefs kf1)
                 (getTimelineKey <$> bonerefs kf2)
      where
        bonerefs k = _mainlineKeyBoneRef k
                  <> _mainlineKeyObjectRef k

    progress = normalize (_mainlineKeyTime kf1)
                         (_mainlineKeyTime kf2)
                         $ frame

    betweenKeyframes (k1, k2) =
      let t1 = _mainlineKeyTime k1
          t2 = _mainlineKeyTime k2
       in (  t1 <= frame
          && t2 >  frame
          ) || ( t1 > t2 && t2 > frame )

    result = accumulate <$> fmap (uncurry lerpBones) tlKeys
      where
        accumulate rb = maybe rb (\x -> result !! x <> rb) $ _rbParent rb

    lerpBones (tlk1, parent, zindex) (tlk2, _, _)
      = let b1 = _timelineKeyBone tlk1
            b2 = _timelineKeyBone tlk2
            spin = fromIntegral $ _timelineKeySpin tlk1
            overEach f g = f (toRealFloat $ g b1)
                             (toRealFloat $ g b2)
            lerping = overEach (lerp progress)
         in ResultBone ( degToRad
                       $ overEach (lerpAngle spin)
                                  _timelineBoneAngle)
                       (lerping _timelineBoneX)
                       (lerping _timelineBoneY)
                       parent
                       (tlk1  ^. timelineKeyBone.timelineBoneObj)
                       zindex


    lerpAngle spin r1 r2 =
      lerp progress r1 (r1 + normalizeDeg r1 r2 * spin)

    normalizeDeg r1 r2 =
      let deg = fmod 360 (max r1 r2 - min r1 r2)
       in case deg > 180 of
            True  -> 360 - deg
            False -> deg

    getTimelineKey :: BoneRef -> (TimelineKey, Maybe Int, Maybe Int)
    getTimelineKey br =
      (, _boneRefParent br, _boneRefZIndex br)
        $  _timelineKey (_animTimeline anim !! _boneRefTimeline br)
        !! _boneRefKey br

fmod :: (Ord a, Num a)
     => a  -- ^ Upper bound.
     -> a  -- ^ Value.
     -> a
fmod r a | a < 0     = fmod r $ a + r
         | a >= r    = fmod r $ a - r
         | otherwise = a

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

