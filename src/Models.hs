{-# LANGUAGE OverloadedStrings #-}

module Models where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Snap.Snaplet.MongoDB
import Database.MongoDB ((=:), (!?), Document)
import qualified Database.MongoDB as M hiding (index)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Monoid
import Data.Maybe
import System.Random
import Data.List
import Data.Char
import Numeric (showHex)
import qualified Text.Email.Validate as Email
import Data.Time
import Data.Text.Encoding

import Types

-- DOCUMENTABLE

(!?!) :: M.Val a => Document -> M.Label -> a
(!?!) a b = fromJust $ a !? b

instance Documentable (Team) where
  fromDoc doc = Team $ doc !?! "name"
  toDoc team = ["name" =: tmName team]

instance Documentable (Student) where
  fromDoc doc = Student (doc !?! "username") (unPrettyPrint $ doc !?! "passhash")
                (encodeUtf8 $ doc !?! "salt") (doc !?! "email") (doc !?! "realname")
                (doc !?! "school") (doc !?! "team")
                (map fromDoc $ doc !?! "psets")
  toDoc s = ["username" =: sUsername s, "passhash" =: prettyPrint (sPassHash s),
             "salt" =: decodeUtf8 (sPassSalt s), "email" =: sEmail s,
             "realname" =: sRealName s, "school" =: sSchool s, "team" =: sTeam s,
             "psets" =: map toDoc (sProblemSets s)]

instance Documentable (ProblemSet) where
  fromDoc doc = ProblemSet (doc !?! "timeassigned") (map fromDoc $ doc !?! "ppairs")
  toDoc pset = ["timeassigned" =: psAssigned pset,
                "ppairs" =: map toDoc (psProblemPairs pset)]

instance Documentable (ProblemPair) where
  fromDoc doc = ProblemPair (toPPS $ doc !?! "pairstatus")
                (doc !?! "lookedatanswer") (map fromDoc $ doc !?! "problems")
    where toPPS :: String -> ProblemPairStatus
          toPPS "notstarted" = PPNotStarted
          toPPS "inprogress" = PPInProgress
          toPPS "done" = PPDone
  toDoc pp = ["pairstatus" =: fromPPS (ppStatus pp),
              "lookedatanswer" =: ppLookedAtAnswer pp,
              "problems" =: map toDoc (ppProblems pp)]
    where fromPPS = (show :: ProblemPairStatus -> String)

instance Documentable (Problem) where
  fromDoc doc = Problem (doc !?! "description") (doc !?! "withintimesolution")
                (doc !?! "withintimetime") (doc !?! "aftertimesolution")
                (doc !?! "aftertimetime")
  toDoc p = ["description" =: (pDescriptionId p),
             "withintimesolution" =: (pWithinTimeSolution p),
             "withintimetime" =: (pWithinTimeTime p),
             "aftertimesolution" =: (pAfterTimeSolution p),
             "aftertimetime" =: (pAfterTimeTime p)]

instance Documentable (ProblemDescription) where
  fromDoc doc = ProblemDescription (doc !?! "text") (doc !?! "solutions")
  toDoc pd = ["text" =: pdText pd, "solutions" =: pdSolutions pd]

-- USER (user authentication, user data validation, user lookup) --

hashPass :: Text -> ByteString -> ByteString
hashPass clear salt = SHA1.hash $ (encodeUtf8 clear) <> salt

makeSalt :: MonadIO m => m ByteString
makeSalt = liftIO $ B.pack <$> replicateM 16 (randomRIO ('a', 'z'))

prettyPrint :: ByteString -> Text
prettyPrint = T.pack . concatMap (\c -> showHex (ord c) "") . B.unpack

unPrettyPrint :: Text -> ByteString
unPrettyPrint = B.pack . map (chr . read . T.unpack . ("0x" <>)) .
                takeWhile (not . T.null) . unfoldr (Just . T.splitAt 2)
{-
createUser :: Text -> Text -> Text -> Text -> Text -> AppHandler User
createUser username clear email realName location = do
  salt <- makeSalt
  let passHash = hashPass clear salt
  return $ User username passHash salt
    email realName location

lookupUserByName :: Text -> AppHandler (DBEither User)
lookupUserByName username = do
  maybeResult <- maybeWithDB $ M.findOne $
                 M.select ["username" =: username] "users"
  case maybeResult of
    Nothing -> return $ Left DBFail
    Just Nothing -> return $ Left ResultFail
    Just (Just doc) -> do
      userObj <- userDocToUser doc
      return $ Right userObj

authenticateUser :: Text -> Text -> AppHandler (DBEither ())
authenticateUser username clear = do
  eitherUser <- lookupUserByName username
  let eitherMatch = do
        user <- eitherUser
        let salt = uPassSalt user
            refPass = uPassHash user
            checkPass = hashPass clear salt
        return (prettyPrint refPass == prettyPrint checkPass)
  return $ case eitherMatch of
    Left err -> Left err
    Right False -> Left ResultFail
    Right True -> Right ()
-}