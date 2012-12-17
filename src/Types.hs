{-# LANGUAGE TemplateHaskell #-}

module Types where

import Snap.Snaplet
import Snap.Snaplet.Session
import Data.Lens.Common
import Data.Lens.Template
import Snap.Snaplet.MongoDB
import Database.MongoDB (Document, Action)
import qualified Control.Category as Cat
import Data.Text (Text)
import Data.Time
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Bson (ObjectId)
import Data.Time
import Control.Monad.Trans


data App = App {
  _sess  :: Snaplet SessionManager,
  _mongo :: Snaplet MongoDB
}
makeLens ''App
instance HasMongoDB App where
  getMongoDB = getL (snapletValue Cat.<<< mongo)

type AppHandler = Handler App App

data DBError = DBFail | ResultFail deriving (Eq, Show)
type DBEither a = Either DBError a

class (MonadIO m) => HasMaybeDB m where
  withDB :: Action IO a -> m (Maybe a)

class Documentable a where
  toDoc :: a -> Document
  fromDoc :: Document -> a


-- MODELS

data Student = Student {
  sUsername :: Text,
  sPassHash :: ByteString,
  sPassSalt :: ByteString,
  sEmail :: Text,
  sRealName :: Text,
  sSchool :: Text,
  sTeam :: ObjectId,
  sProblemSets :: [ProblemSet]
  } deriving (Eq, Show)

data ProblemSet = ProblemSet {
  psAssigned :: UTCTime,
  psProblemPairs :: [ProblemPair]
  } deriving (Eq, Show)

data ProblemPair = ProblemPair {
  ppStatus :: ProblemPairStatus,
  ppLookedAtAnswer :: Bool,
  ppProblems :: [Problem]
  } deriving (Eq, Show)

data ProblemPairStatus = PPNotStarted | PPInProgress | PPDone
                       deriving (Eq, Ord, Enum)
instance Show (ProblemPairStatus) where
  show PPNotStarted = "notstarted"
  show PPInProgress = "inprogress"
  show PPDone = "done"

data Problem = Problem {
  pDescriptionId :: ObjectId,
  pWithinTimeSolution :: Text,
  pWithinTimeTime :: UTCTime,
  pAfterTimeSolution :: Text,
  pAfterTimeTime :: UTCTime
  } deriving (Eq, Show)

data Team = Team {
  tmName :: Text
  } deriving (Eq, Show)

data ProblemDescription = ProblemDescription {
  pdText :: Text,
  pdSolutions :: [Text]
  } deriving (Eq, Show)