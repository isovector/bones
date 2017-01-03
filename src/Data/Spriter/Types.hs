{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Spriter.Types where

import Control.Applicative ((<|>))
import Control.Lens.TH
import Control.Monad (guard, mzero)
import Data.Aeson
import Data.Aeson.Casing
import Data.Scientific
import GHC.Generics

data Schema = Schema
  { _schemaEntity :: [Entity]
  , _schemaFolder :: [Folder]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Schema where
   toJSON = genericToJSON $ aesonDrop 7 snakeCase
instance FromJSON Schema where
   parseJSON = genericParseJSON $ aesonDrop 7 snakeCase

data Entity = Entity
  { _entityAnimation :: [Animation]
  , _entityId        :: Int
  , _entityName      :: String
  , _entityObjInfo   :: [ObjInfo]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Entity where
   toJSON = genericToJSON $ aesonDrop 7 snakeCase
instance FromJSON Entity where
   parseJSON = genericParseJSON $ aesonDrop 7 snakeCase

data Animation = Animation
  { _animId       :: Int
  , _animInterval :: Int
  , _animLength   :: Int  -- ^ Number of frames.
  , _animName     :: String
  , _animMainline :: Mainline
  , _animTimeline :: [Timeline]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Animation where
   toJSON = genericToJSON $ aesonDrop 5 snakeCase
instance FromJSON Animation where
   parseJSON = genericParseJSON $ aesonDrop 5 snakeCase

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
  , _mainlineKeyTime      :: Int
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
  , _timelineKeyTime   :: Int
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

data TimelineBone = TimelineBone
  { _timelineBoneAngle :: Scientific
  , _timelineBoneX     :: Scientific
  , _timelineBoneY     :: Scientific
  , _timelineBoneObj   :: Maybe BoneObj
  } deriving (Eq, Show, Read, Generic)

instance FromJSON TimelineBone where
  parseJSON v = flip (withObject "TimelineBone") v $ \obj ->
    TimelineBone <$> (maybe 0 id <$> obj .:? "angle")
                 <*> (maybe 0 id <$> obj .:? "x")
                 <*> (maybe 0 id <$> obj .:? "y")
                 <*> (return . hush $ fromJSON v)

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

