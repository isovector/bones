{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Spriter.Skeleton
  ( loadSchema
  , ResultBone (..)
  , isBone
  , animate
  , fmod
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Aeson (eitherDecodeFileStrict)
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
import           Data.Spriter.Types
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import Data.Align (align)
import Data.These

loadSchema :: FilePath -> IO (Either String Schema)
loadSchema = eitherDecodeFileStrict


data ResultBone = ResultBone
  { _rbAngle   :: Double
  , _rbX       :: Double
  , _rbY       :: Double
  , _rbScaleX  :: Double
  , _rbScaleY  :: Double
  , _rbParent  :: Maybe Int
  , _rbObj     :: Maybe BoneObj
  , _rbObjInfo :: Maybe ObjInfo
  , _rbZIndex  :: Maybe Int
  } deriving (Eq, Show, Read)

isBone :: ResultBone -> Bool
isBone = not . isJust . _rbObj

instance Semigroup ResultBone where
  ResultBone a x y sx sy _ _ _ _ <> ResultBone a' x' y' sx' sy' p o' oi' z' =
    let s = sin a
        c = cos a
        x'' = (x' * c) - (y' * s)
        y'' = (x' * s) + (y' * c)
     in ResultBone (a + a') (x + x'') (y + y'') (sx * sx') (sy * sy') p (o') (oi') (z')

instance Monoid ResultBone where
  mempty  = ResultBone 0 0 0 1 1 Nothing Nothing Nothing Nothing

animate :: Entity
        -> AnimationName
        -> Double  -- ^ Frame.
        -> Maybe [ResultBone]
animate ent aname frame =
  case frame <= anim ^. animLength of
    True  -> Just $ sortBy (comparing _rbZIndex) result
    False -> Nothing
  where
    anim :: Animation
    Just anim = ent ^. entityAnimation . at aname

    keyframes :: [MainlineKey]
    keyframes = anim ^. animMainline . mainlineKey

    allKeyframes :: [MainlineKey]
    allKeyframes = (head keyframes : keyframes)
                ++ [head keyframes & mainlineKeyTime .~ _animLength anim]

    kf1, kf2 :: MainlineKey
    (kf1, kf2) = head . filter (betweenKeyframes frame)
                      . zip allKeyframes
                      $ tail allKeyframes

    objs :: Map Text ObjInfo
    objs = M.fromList $ do
      oi <- _entityObjInfo ent
      pure (_objInfoName oi, oi)

    tlKeys :: [These TimelineKeyStuff TimelineKeyStuff]
    tlKeys = align (getTimelineKey objs anim <$> bonerefs kf1)
                   (getTimelineKey objs anim <$> bonerefs kf2)

    progress :: Double
    progress = normalize (_mainlineKeyTime kf1)
                         (_mainlineKeyTime kf2)
                         $ frame

    result :: [ResultBone]
    result = accumulate <$> fmap (lerpBones progress) tlKeys
      where
        accumulate rb = maybe rb (\x -> result !! x <> rb) $ _rbParent rb

betweenKeyframes :: Double -> (MainlineKey, MainlineKey) -> Bool
betweenKeyframes frame (k1, k2) =
  let t1 = _mainlineKeyTime k1
      t2 = _mainlineKeyTime k2
    in (  t1 <= frame
      && t2 >  frame
      ) || ( t1 > t2 && t2 > frame )

bonerefs :: MainlineKey -> [BoneRef]
bonerefs k = _mainlineKeyBoneRef k
          <> _mainlineKeyObjectRef k

data TimelineKeyStuff = TimelineKeyStuff
  { tks_timelinekey    :: TimelineKey
  , tks_parent_boneref :: Maybe Int
  , tks_zindex         :: Maybe Int
  , tks_object         :: Maybe ObjInfo
  }

getTimelineKey :: Map Text ObjInfo -> Animation -> BoneRef -> TimelineKeyStuff
getTimelineKey objs anim br =
  let timeline = _animTimeline anim !! _boneRefTimeline br
   in TimelineKeyStuff
        (_timelineKey timeline !! _boneRefKey br)
        (_boneRefParent br)
        (_boneRefZIndex br)
        (M.lookup (_timelineName timeline) objs)

lerpBones
    :: Double
    -> These TimelineKeyStuff TimelineKeyStuff
    -> ResultBone
lerpBones progress (These tks1@(TimelineKeyStuff tlk1 _ _ _) tks2)
  = let b1 = _timelineKeyBone tlk1
        b2 = _timelineKeyBone $ tks_timelinekey tks2
        spin = fromIntegral $ _timelineKeySpin tlk1
        overEach f g = f (g b1)
                         (g b2)
        lerping = overEach (lerp progress)
      in ResultBone ( degToRad
                    $ overEach (lerpAngle progress spin)
                              _timelineBoneAngle)
                    (lerping _timelineBoneX)
                    (lerping _timelineBoneY)
                    (lerping _timelineBoneScaleX)
                    (lerping _timelineBoneScaleY)
                    (tks_parent_boneref tks1)
                    (tlk1  ^. timelineKeyBone.timelineBoneObj)
                    (tks_object tks1)
                    (tks_zindex tks1)
lerpBones progress (This tks1) = lerpBones progress (These tks1 tks1)
lerpBones progress (That tks1) = mempty


lerpAngle :: Double -> Double -> Double -> Double -> Double
lerpAngle progress spin r1 r2 =
  lerp progress r1 (r1 + normalizeDeg r1 r2 * spin)


normalizeDeg :: Double -> Double -> Double
normalizeDeg r1 r2 =
  let deg = fmod 360 (max r1 r2 - min r1 r2)
    in case deg > 180 of
        True  -> 360 - deg
        False -> deg

fmod :: (Fractional a, RealFrac a)
     => a  -- ^ Upper bound.
     -> a  -- ^ Value.
     -> a
fmod r a = a - (fromIntegral (floor (a / r) :: Int) * r)

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

