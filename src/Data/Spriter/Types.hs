{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Spriter.Types where

import           Control.Applicative ((<|>))
import           Control.Lens.TH
import           Control.Monad (guard, mzero)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V
import           Data.Scientific
import           Data.String (IsString (..))
import           GHC.Generics


newtype EventName = EventName String
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, Ord)

instance IsString EventName where
  fromString = EventName

newtype EntityName = EntityName String
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, Ord)

instance FromJSON (Map EntityName Entity) where
  parseJSON = withArray "EntityMap" $ \arr -> do
    entities <- sequence $ fmap parseJSON arr
    return . M.fromList
           . fmap ((,) =<< EntityName . _entityName)
           $ V.toList entities

instance ToJSON (Map EntityName Entity) where
  toJSON = toJSON . fmap snd . M.toList

instance IsString EntityName where
  fromString = EntityName

data Schema = Schema
  { _schemaEntity :: Map EntityName Entity
  , _schemaFolder :: [Folder]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Schema where
   toJSON = genericToJSON $ aesonDrop 7 snakeCase
instance FromJSON Schema where
   parseJSON = genericParseJSON $ aesonDrop 7 snakeCase

newtype AnimationName = AnimationName String
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, Ord)

instance FromJSON (Map AnimationName Animation) where
  parseJSON = withArray "AnimationMap" $ \arr -> do
    anims <- sequence $ fmap parseJSON arr
    return . M.fromList
           . fmap ((,) =<< AnimationName . _animName)
           $ V.toList anims

instance ToJSON (Map AnimationName Animation) where
  toJSON = toJSON . fmap snd . M.toList

instance IsString AnimationName where
  fromString = AnimationName

data Entity = Entity
  { _entityAnimation :: Map AnimationName Animation
  , _entityId        :: Int
  , _entityName      :: String
  , _entityObjInfo   :: [ObjInfo]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Entity where
   toJSON = genericToJSON $ aesonDrop 7 snakeCase
instance FromJSON Entity where
   parseJSON = genericParseJSON $ aesonDrop 7 snakeCase

data Animation = Animation
  { _animId        :: Int
  , _animInterval  :: Int
  , _animLength    :: Double  -- ^ Number of frames.
  , _animName      :: String
  , _animMainline  :: Mainline
  , _animTimeline  :: [Timeline]
  , _animEventline :: [Eventline]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Animation where
   toJSON = genericToJSON $ aesonDrop 5 snakeCase

instance FromJSON Animation where
  parseJSON = withObject "Animation" $ \obj ->
    Animation <$> obj .: "id"
              <*> obj .: "interval"
              <*> obj .: "length"
              <*> obj .: "name"
              <*> obj .: "mainline"
              <*> obj .: "timeline"
              <*> (maybe [] id <$> obj .:? "eventline")

data Eventline = Eventline
  { _eventlineId   :: Int
  , _eventlineKey  :: [EventlineKey]
  , _eventlineName :: EventName
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Eventline where
   toJSON = genericToJSON $ aesonDrop 10 snakeCase
instance FromJSON Eventline where
   parseJSON = genericParseJSON $ aesonDrop 10 snakeCase

data EventlineKey = EventlineKey
  { _eventlineKeyId     :: Int
  , _eventlineKeyTime   :: Double
  } deriving (Eq, Show, Read, Generic)

instance ToJSON EventlineKey where
   toJSON = genericToJSON $ aesonDrop 13 snakeCase
instance FromJSON EventlineKey where
   parseJSON = genericParseJSON $ aesonDrop 13 snakeCase

data Mainline = Mainline
  { _mainlineKey :: [MainlineKey]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Mainline where
   toJSON = genericToJSON $ aesonDrop 9 snakeCase
instance FromJSON Mainline where
   parseJSON = genericParseJSON $ aesonDrop 9 snakeCase

data MainlineKey = MainlineKey
  { _mainlineKeyBoneRef   :: [BoneRef]
  , _mainlineKeyId        :: Int
  , _mainlineKeyObjectRef :: [BoneRef]
  , _mainlineKeyTime      :: Double
  } deriving (Eq, Show, Read, Generic)

instance FromJSON MainlineKey where
  parseJSON = withObject "MainlineKey" $ \obj ->
    MainlineKey <$> obj .: "bone_ref"
                <*> obj .: "id"
                <*> obj .: "object_ref"
                <*> (maybe 0 id <$> obj .:? "time")

instance ToJSON MainlineKey where
   toJSON = genericToJSON $ aesonDrop 12 snakeCase

data BoneRef = BoneRef
  { _boneRefId       :: Int
  , _boneRefKey      :: Int
  , _boneRefParent   :: Maybe Int
  , _boneRefTimeline :: Int
  , _boneRefZIndex   :: Maybe Int
  } deriving (Eq, Show, Read, Generic)

instance FromJSON BoneRef where
  parseJSON = withObject "BoneRef" $ \obj ->
    BoneRef <$> obj .: "id"
            <*> obj .: "key"
            <*> obj .:? "parent"
            <*> possiblyStringButShouldBeInt obj "timeline"
            <*> (fmap read <$> obj .:? "z_index")
   where
    possiblyStringButShouldBeInt obj key = do
      val <- obj .: key
      let int = hush $ fromJSON val
          str = hush $ fromJSON val

      maybe mempty return $ int <|> fmap read str

instance ToJSON BoneRef where
   toJSON = genericToJSON $ aesonDrop 8 snakeCase

data Timeline = Timeline
  { _timelineId   :: Int
  , _timelineKey  :: [TimelineKey]
  , _timelineName :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Timeline where
   toJSON = genericToJSON $ aesonDrop 9 snakeCase
instance FromJSON Timeline where
   parseJSON = genericParseJSON $ aesonDrop 9 snakeCase

data TimelineKey = TimelineKey
  { _timelineKeyId     :: Int
  , _timelineKeyBone   :: TimelineBone
  , _timelineKeySpin   :: Int
  , _timelineKeyTime   :: Double
  } deriving (Eq, Show, Read, Generic)

instance FromJSON TimelineKey where
  parseJSON = withObject "TimelineKey" $ \obj -> do
    bone   <- obj .:? "bone"
    o <- obj .:? "object"
    tlbone <- maybe mzero return $ o <|> bone

    TimelineKey <$> obj .: "id"
                <*> pure tlbone
                <*> (maybe 1 id <$> obj .:? "spin")
                <*> (maybe 0 id <$> obj .:? "time")

instance ToJSON TimelineKey where
   toJSON = genericToJSON $ aesonDrop 12 snakeCase

data ObjectType = SpriterObject
                | SpriterBone
                | SpriterPoint
                deriving (Eq, Show, Read, Generic)

instance ToJSON ObjectType where
  toJSON SpriterObject = String "object"
  toJSON SpriterBone   = String "bone"
  toJSON SpriterPoint  = String "point"

instance FromJSON ObjectType where
  parseJSON (String "object") = pure SpriterObject
  parseJSON (String "bone")   = pure SpriterBone
  parseJSON (String "point")  = pure SpriterPoint
  parseJSON _                 = mzero

data TimelineBone = TimelineBone
  { _timelineBoneAngle  :: Scientific
  , _timelineBoneX      :: Scientific
  , _timelineBoneY      :: Scientific
  , _timelineBoneScaleX :: Scientific
  , _timelineBoneScaleY :: Scientific
  , _timelineBoneObj    :: Maybe BoneObj
  , _timelineObjType    :: ObjectType
  } deriving (Eq, Show, Read, Generic)

instance FromJSON TimelineBone where
  parseJSON v = flip (withObject "TimelineBone") v $ \obj ->
    TimelineBone <$> (maybe 0 id <$> obj .:? "angle")
                 <*> (maybe 0 id <$> obj .:? "x")
                 <*> (maybe 0 id <$> obj .:? "y")
                 <*> (maybe 1 id <$> obj .:? "scale_x")
                 <*> (maybe 1 id <$> obj .:? "scale_y")
                 <*> (return . hush $ fromJSON v)
                 <*> (maybe SpriterBone id <$> obj .:? "object_type")

hush :: Result a -> Maybe a
hush (Error _)   = Nothing
hush (Success a) = Just a

instance ToJSON TimelineBone where
   toJSON = genericToJSON $ aesonDrop 13 snakeCase

data BoneObj = BoneObj
  { _boneObjFile   :: Int
  , _boneObjFolder :: Int
  } deriving (Eq, Show, Read, Generic)

instance ToJSON BoneObj where
   toJSON = genericToJSON $ aesonDrop 8 snakeCase
instance FromJSON BoneObj where
   parseJSON = genericParseJSON $ aesonDrop 8 snakeCase

data ObjInfo = Bone
  { _boneName   :: String
  , _boneWidth  :: Scientific
  , _boneHeight :: Scientific
  } deriving (Eq, Show, Read, Generic)

instance ToJSON ObjInfo where
  toJSON o = object [ "name" .= _boneName o
                    , "w" .= _boneWidth o
                    , "h" .= _boneHeight o
                    , "type" .= ("bone" :: String)
                    ]

instance FromJSON ObjInfo where
  parseJSON = withObject "ObjInfo" $ \obj -> do
    t :: String <- obj .: "type"
    guard $ t == "bone"
    Bone <$> obj .: "name"
         <*> obj .: "w"
         <*> obj .: "h"

data Folder = Folder
  { _folderId   :: Int
  , _folderFile :: [File]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Folder where
   toJSON = genericToJSON $ aesonDrop 7 snakeCase
instance FromJSON Folder where
   parseJSON = genericParseJSON $ aesonDrop 7 snakeCase

data File = File
  { _fileId     :: Int
  , _fileName   :: FilePath
  , _filePivotX :: Int
  , _filePivotY :: Int
  , _fileWidth  :: Int
  , _fileHeight :: Int
  } deriving (Eq, Show, Read, Generic)

instance ToJSON File where
   toJSON = genericToJSON $ aesonDrop 5 snakeCase
instance FromJSON File where
   parseJSON = genericParseJSON $ aesonDrop 5 snakeCase

makeLenses ''Animation
makeLenses ''BoneObj
makeLenses ''BoneRef
makeLenses ''Entity
makeLenses ''File
makeLenses ''Folder
makeLenses ''Mainline
makeLenses ''MainlineKey
makeLenses ''ObjInfo
makeLenses ''Schema
makeLenses ''Timeline
makeLenses ''TimelineBone
makeLenses ''TimelineKey

