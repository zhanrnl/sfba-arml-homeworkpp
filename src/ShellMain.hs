{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleInstances, TypeSynonymInstances #-}

module Main where

import Database.MongoDB as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Bson (ObjectId)
import Data.Time
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Text.InterpolatedString.Perl6
import System.IO
import System.Exit
import Data.Monoid ((<>))
import Data.Maybe
import Control.Arrow
import Text.Printf
import Data.List (elemIndex)

import Types hiding (App)
import Models

type Environment = Pipe
type App = ReaderT Environment IO

instance HasMaybeDB (App) where
  withDB = withDB'

withDB' :: Action IO a -> App (Maybe a)
withDB' action = do
  pipe <- ask
  liftIO $ toMaybe <$> access pipe master "homeworkpp" action

toMaybe :: Either a b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right b) = Just b

ioPrint :: MonadIO m => Text -> m ()
ioPrint str = liftIO $ do
  putStr $ T.unpack str
  hFlush stdout
  
ioPrintLn :: MonadIO m => Text -> m ()
ioPrintLn str = liftIO $ do
  putStrLn $ T.unpack str

ioInput :: MonadIO m => m Text
ioInput = liftIO $ T.pack <$> getLine

maybeRead :: Read a => Text -> Maybe a
maybeRead = fmap fst . listToMaybe . reads . T.unpack

getInteger :: (Functor m, MonadIO m) => Int -> Int -> Text -> m Int
getInteger minVal maxVal prompt = do
  ioPrint prompt
  mN <- (maybeRead :: Text -> Maybe Int) <$> ioInput
  case mN of
    Nothing -> do
      ioPrintLn "Not an integer, try again."
      getInteger minVal maxVal prompt
    Just n -> f n 
  where f n 
          | n >= minVal && n <= maxVal = return n
          | otherwise = do
            ioPrintLn [qc|Out of range, must be >= {minVal} and <= {maxVal}.|]
            getInteger minVal maxVal prompt

getIntegerDefault :: (Functor m, MonadIO m) => Int -> Int -> Text -> Int -> m Int
getIntegerDefault minVal maxVal prompt def = do
  ioPrint [qc|{prompt} (default {def}) |]
  rawIn <- ioInput
  let mN = (maybeRead :: Text -> Maybe Int) rawIn
  case mN of
    Nothing -> do
      if (rawIn == "") then do
        return def
        else do
        ioPrintLn "Not an integer, try again."
        getInteger minVal maxVal prompt
    Just n -> f n 
  where f n 
          | n >= minVal && n <= maxVal = return n
          | otherwise = do
            ioPrintLn [qc|Out of range, must be >= {minVal} and <= {maxVal}.|]
            getInteger minVal maxVal prompt

getYesNo :: (MonadIO m) => Text -> m Bool
getYesNo prompt = do
  ioPrint prompt
  t <- ioInput
  if t == "yes" then return True else
    if t == "no" then return False else do
      ioPrintLn "Please enter \"yes\" or \"no\"."
      getYesNo prompt

promptDefault :: (MonadIO m) => Text -> Text -> m Text
promptDefault prompt def = do
  ioPrint [qc|{prompt} (default {def}) |]
  entered <- ioInput
  if entered == ""
    then return def
    else return entered

shellPrompt :: Text
shellPrompt = "\n> "

intro :: Text
intro = T.replicate 80 "-" <> [q|
Welcome to the SFBA ARML Homework++ admin shell.
Created by Lennart Jansson, complain to lennartj@stanford.edu if there are 
problems.

Enter "help" for help information.
|]

processInput :: Text -> App ()
processInput "help" =
  ioPrint [q|Use the Homework++ admin shell to add or modify students, create and
assign problem sets, and log attendence.

Supported commands:
help - print this help text

teams - list all teams
add team - interactive prompt to create a new team
delete team - interactive prompt to delete a team

students - list all students
add student - interactive prompt to create a new student
delete student - interactive prompt to delete a student
edit student - interactive prompt to edit a student

quit - leave the shell
exit - same as quit
|]

processInput "teams" = printNumberedTeams >> (return ())

processInput "add team" = do
  ioPrint "Enter new team name: "
  newName <- ioInput
  let newTeam = Team newName
  mResult <- withDB $ M.insert "teams" $ toDoc newTeam
  if isJust mResult
    then ioPrintLn "Added team."
    else ioPrintLn "Error adding team."

processInput "delete team" = do
  numberedTeams <- printNumberedTeams
  let numTeams = length numberedTeams
  if (numTeams /= 0) then do
    n <- getInteger 0 (numTeams-1) "Enter a number corresponding to a team above: "
    let teamNameToDelete = (tmName . snd) $ numberedTeams !! n
    b <- getYesNo [qc|Really delete team {teamNameToDelete}? |]
    when b $ do
      mResult2 <- withDB $ M.delete $ M.select ["name" =: teamNameToDelete] "teams"
      if isJust mResult2
        then ioPrintLn "Deleted team."
        else ioPrintLn "Error deleting team."
    else ioPrintLn "No teams."

processInput "add student" = do
  ioPrint "Enter student username: "
  username <- ioInput
  ioPrint "Enter student password: "
  clearPass <- ioInput
  ioPrint "Enter student email: "
  email <- ioInput
  ioPrint "Enter student real name: "
  realName <- ioInput
  ioPrint "Enter student's school: "
  school <- ioInput
  numberedTeams <- printNumberedTeams
  let numTeams = length numberedTeams
  if (numTeams /= 0) then do
    n <- getInteger 0 (numTeams-1) "Enter a number corresponding to a team above: "
    let selectedTeamName = (tmName . snd) $ numberedTeams !! n
    teamId <- fromJust <$> getTeamId selectedTeamName
    salt <- makeSalt
    let passHash = hashPass clearPass salt
    let student = Student username passHash salt email realName school teamId []
    mResult <- withDB $ M.insert "students" $ toDoc student
    if isJust mResult
      then ioPrintLn "Added student."
      else ioPrintLn "Error adding student."
    else ioPrintLn [q|No teams, can't create students until there are teams for students
to be added to.|]

processInput "students" = printNumberedStudents >> return ()

processInput "delete student" = do
  numberedStudents <- printNumberedStudents
  let numStudents = length numberedStudents
  if (numStudents /= 0) then do
    n <- getInteger 0 (numStudents-1)
         "Enter a number corresponding to a student above: "
    let studentNameToDelete = (sUsername . snd) $ numberedStudents !! n
    b <- getYesNo [qc|Really delete student {studentNameToDelete}? |]
    when b $ do
      mResult2 <- withDB $ M.delete $ M.select
                  ["username" =: studentNameToDelete] "students"
      if isJust mResult2
        then ioPrintLn "Deleted student."
        else ioPrintLn "Error deleting student."
    else ioPrintLn "No students."

processInput "edit student" = do
  numberedStudents <- printNumberedStudents
  let numStudents = length numberedStudents
  if (numStudents /= 0) then do
    n <- getInteger 0 (numStudents-1)
         "Enter a number corresponding to a student above: "
    let student = snd $ numberedStudents !! n
        oldUsername = sUsername student
    oldTeamName <- fromJust <$> (getTeamName $ sTeam student)
    ioPrintLn [q|At each prompt, press enter without typing anything to just use the
default (already set) value.|]
    newUsername <- promptDefault "Enter student username: " (sUsername student)
    newEmail <- promptDefault "Enter student email: " (sEmail student)
    newRealName <- promptDefault "Enter student real name: " (sRealName student)
    newSchool <- promptDefault "Enter student school: " (sSchool student)
    numberedTeams <- printNumberedTeams
    let numTeams = length numberedTeams
    tn <- getIntegerDefault 0 (numTeams-1) "Enter a number corresponding to a team above: " (fromJust $ elemIndex oldTeamName $ map (tmName . snd) numberedTeams)
    let selectedTeamName = (tmName . snd) $ numberedTeams !! tn
    newTeamId <- fromJust <$> getTeamId selectedTeamName
    mResult <- withDB $ M.modify (M.select ["username" =: oldUsername] "students")
               ["$set" =: ["username" =: newUsername, "email" =: newEmail,
                           "realname" =: newRealName, "school" =: newSchool,
                           "team" =: newTeamId]]
    if isJust mResult
      then ioPrintLn "Edited student."
      else ioPrintLn "Error editing student."
    else ioPrintLn "No students."
    

processInput "quit" = do
  ioPrintLn "Goodbye"
  liftIO $ exitSuccess

processInput "exit" = processInput "quit"

processInput c = 
  ioPrint [qc|Unknown command {c}. Enter "help" for usage.
|]

-- HELPERS

printNumberedTeams :: (HasMaybeDB m) => m [(Int, Team)]
printNumberedTeams = do
  mResult <- withDB $ do
    cursor <- M.find (M.select [] "teams"){M.sort = ["name" =: (1::Int)]}
    M.rest cursor
  case mResult of
    Nothing -> do
      ioPrintLn "No teams found."
      return []
    Just result -> do
      let numberedTeams = zip ([0..]::[Int]) $ map fromDoc result
      mapM_ (\(a,b) -> ioPrintLn [qc|[{a}]: {tmName b}|]) numberedTeams
      return numberedTeams

printNumberedStudents :: (HasMaybeDB m) => m [(Int, Student)]
printNumberedStudents = do
  mResult <- withDB $ do
    cursor <- M.find (M.select [] "students")
    M.rest cursor
  case mResult of
    Just result -> do
      ioPrintLn $ T.pack $ printf "       %-20s %-25s %-25s %-25s %-5s"
        ("Username"::String) ("Email"::String) ("Name"::String)
        ("School"::String) ("Team"::String)
      let numberedStudents = zip ([0..]::[Int]) $ map fromDoc result
      mapM_ (ioPrintLn <=< (\(a, b) -> formatStudent a b)) numberedStudents
      return numberedStudents
    Nothing -> ioPrintLn "No students found." >> return []
  where formatStudent :: (HasMaybeDB m) => Int -> Student -> m Text
        formatStudent i s = do
          teamName <- getTeamName (sTeam s)
          return $ T.pack $ (printf "%-6s %-20s %-25s %-25s %-25s %-5s"
                             ([qc|[{show i}]: |] :: String)
                             (T.unpack $ sUsername s) (T.unpack $ sEmail s)
                             (T.unpack $ sRealName s) (T.unpack $ sSchool s)
                             (maybe "" T.unpack teamName))

getTeamId :: (HasMaybeDB m) => Text -> m (Maybe ObjectId)
getTeamId teamName = do
  mResult <- withDB $ M.findOne (M.select ["name" =: teamName] "teams")
  case mResult of
    Just (Just doc) -> return $ doc !? "_id"
    _ -> return Nothing

getTeamName :: (HasMaybeDB m) => ObjectId -> m (Maybe Text)
getTeamName teamId = do
  mResult <- withDB $ M.findOne (M.select ["_id" =: teamId] "teams")
  case mResult of
    Just (Just doc) -> return $ doc !? "name"
    _ -> return Nothing

-- APP LOOP

runApp :: App ()
runApp = do
  ioPrint intro
  appLoop

appLoop :: App ()
appLoop = do
  ioPrint shellPrompt
  command <- ioInput
  processInput command
  appLoop

main :: IO ()
main = do
  pipe <- runIOE $ connect (host "127.0.0.1")
  runReaderT runApp pipe
  close pipe