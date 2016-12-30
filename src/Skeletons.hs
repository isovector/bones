{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Skeletons where

import Control.Lens.TH
import Data.String.Conv (toS)
import Data.Aeson
import Control.Monad (guard)
import Data.Aeson.Casing
import GHC.Generics
import Data.Scientific

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
  , _entityCharacterMap :: [()]
  , _entityId :: Int
  , _entityName :: String
  , _entityObjInfo :: [ObjInfo]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Entity where
   toJSON = genericToJSON $ aesonDrop 7 snakeCase
instance FromJSON Entity where
   parseJSON = genericParseJSON $ aesonDrop 7 snakeCase

data Animation = Animation
  { _animId :: Int
  , _animInterval :: Int
  , _animLength :: Int  -- ^ Number of frames.
  , _animName :: String
  -- , _animMainline :: Mainline
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
  { _mainlineKeyBoneRef :: [BoneRef]
  , _mainlineKeyId :: Int
  , _mainlineKeyObjectRef :: [ObjectRef]
  , _mainlineKeyTime :: Int
  } deriving (Eq, Show, Read, Generic)

instance ToJSON MainlineKey where
   toJSON = genericToJSON $ aesonDrop 12 snakeCase
instance FromJSON MainlineKey where
   parseJSON = genericParseJSON $ aesonDrop 12 snakeCase

data BoneRef = BoneRef
  { _boneRefId :: Int
  , _boneRefKey :: Int
  , _boneRefTimeline :: Int
  } deriving (Eq, Show, Read, Generic)

instance ToJSON BoneRef where
   toJSON = genericToJSON $ aesonDrop 8 snakeCase
instance FromJSON BoneRef where
   parseJSON = genericParseJSON $ aesonDrop 8 snakeCase

data ObjectRef = ObjectRef
  { _objectRefId :: Int
  , _objectRefKey :: Int
  , _objectRefParent :: Int
  , _objectRefTimeline :: String
  , _objectRefZIndex :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJSON ObjectRef where
   toJSON = genericToJSON $ aesonDrop 10 snakeCase
instance FromJSON ObjectRef where
   parseJSON = genericParseJSON $ aesonDrop 10 snakeCase

data Timeline = Timeline
  { _timelineId :: Int
  , _timelineKey :: [TimelineKey]
  , _timelineName :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Timeline where
   toJSON = genericToJSON $ aesonDrop 9 snakeCase
instance FromJSON Timeline where
   parseJSON = genericParseJSON $ aesonDrop 9 snakeCase

data TimelineKey = TimelineKey
  { _timelineKeyId :: Int
  , _timelineKeyObject :: Maybe TimelineObject
  , _timelineKeyBone :: Maybe TimelineBone
  , _timelineKeySpin :: Maybe Int
  , _timelineKeyTime :: Maybe Int
  } deriving (Eq, Show, Read, Generic)

instance ToJSON TimelineKey where
   toJSON = genericToJSON $ aesonDrop 12 snakeCase
instance FromJSON TimelineKey where
   parseJSON = genericParseJSON $ aesonDrop 12 snakeCase

data TimelineBone = TimelineBone
  { _timelineBoneAngle :: Scientific
  , _timelineBoneX :: Scientific
  , _timelineBoneY :: Scientific
  } deriving (Eq, Show, Read, Generic)

instance ToJSON TimelineBone where
   toJSON = genericToJSON $ aesonDrop 13 snakeCase
instance FromJSON TimelineBone where
   parseJSON = genericParseJSON $ aesonDrop 13 snakeCase

data TimelineObject = TimelineObject
  { _timelineObjAngle :: Scientific
  , _timelineObjFile :: Int
  , _timelineObjFolder :: Int
  , _timelineObjX :: Scientific
  , _timelineObjY :: Scientific
  } deriving (Eq, Show, Read, Generic)

instance ToJSON TimelineObject where
   toJSON = genericToJSON $ aesonDrop 12 snakeCase
instance FromJSON TimelineObject where
   parseJSON = genericParseJSON $ aesonDrop 12 snakeCase

data ObjInfo = Bone
  { _boneName :: String
  , _boneWidth :: Scientific
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
  { _folderId :: Int
  , _folderFile :: [File]
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Folder where
   toJSON = genericToJSON $ aesonDrop 7 snakeCase
instance FromJSON Folder where
   parseJSON = genericParseJSON $ aesonDrop 7 snakeCase

data File = File
  { _fileId :: Int
  , _fileName :: FilePath
  , _filePivotX :: Int
  , _filePivotY :: Int
  , _fileWidth :: Int
  , _fileHeight :: Int
  } deriving (Eq, Show, Read, Generic)

instance ToJSON File where
   toJSON = genericToJSON $ aesonDrop 5 snakeCase
instance FromJSON File where
   parseJSON = genericParseJSON $ aesonDrop 5 snakeCase


test :: IO ()
test = do
  file <- readFile "/home/bootstrap/Projects/bones/basic-anim.scon"
  let Just x = decode $ toS file
  print $ (fromJSON x :: Result Schema)

makeLenses ''Animation
makeLenses ''BoneRef
makeLenses ''Entity
makeLenses ''File
makeLenses ''Folder
makeLenses ''Mainline
makeLenses ''MainlineKey
makeLenses ''ObjInfo
makeLenses ''ObjectRef
makeLenses ''Schema
makeLenses ''Timeline
makeLenses ''TimelineBone
makeLenses ''TimelineKey
makeLenses ''TimelineObject

