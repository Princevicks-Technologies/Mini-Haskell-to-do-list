-- File: Main.hs
module Main where

import CLI (runApp)

main :: IO ()
main = runApp


-- File: Task.hs
{-# LANGUAGE DeriveGeneric #-}

module Task where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (Day)

-- | Task priority levels.
data Priority = Low | Medium | High
  deriving (Show, Read, Eq, Ord, Generic)

-- | Task completion status.
data Status = Pending | Completed
  deriving (Show, Read, Eq, Generic)

-- | Task data structure.
data Task = Task
  { taskId      :: Int
  , title       :: String
  , description :: String
  , dueDate     :: Maybe Day
  , priority    :: Priority
  , status      :: Status
  } deriving (Show, Generic)

instance ToJSON Task
instance FromJSON Task
instance ToJSON Priority
instance FromJSON Priority
instance ToJSON Status
instance FromJSON Status


-- File: Storage.hs
module Storage where

import Task
import System.Directory (doesFileExist)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B
import Control.Exception (catch, IOException)

filePath :: FilePath
filePath = "tasks.json"

-- | Load tasks from JSON file.
loadTasks :: IO [Task]
loadTasks = do
  exists <- doesFileExist filePath
  if not exists
    then return []
    else catch
      (do content <- B.readFile filePath
          case decode content of
            Just tasks -> return tasks
            Nothing    -> return [])
      handler
  where
    handler :: IOException -> IO [Task]
    handler _ = return []

-- | Save tasks to JSON file.
saveTasks :: [Task] -> IO ()
saveTasks tasks = B.writeFile filePath (encode tasks)


-- File: CLI.hs
module CLI where

import Task
import Storage
import Utils

import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime, utctDay)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | Start the To-Do application.
runApp :: IO ()
runApp = do
  putStrLn "Welcome to Haskell To-Do List App"
  menuLoop

-- | Main menu loop.
menuLoop :: IO ()
menuLoop = do
  putStrLn "\nChoose an option:"
  putStrLn "1. Add Task"
  putStrLn "2. View Tasks"
  putStrLn "3. Update Task"
  putStrLn "4. Complete Task"
  putStrLn "5. Delete Task"
  putStrLn "6. Filter Tasks"
  putStrLn "7. Exit"
  putStr ">> " >> hFlush stdout
  choice <- getLine
  case choice of
    "1" -> addTask >> menuLoop
    "2" -> viewTasks >> menuLoop
    "3" -> updateTask >> menuLoop
    "4" -> completeTask >> menuLoop
    "5" -> deleteTask >> menuLoop
    "6" -> filterMenu >> menuLoop
    "7" -> putStrLn "Goodbye!"
    _   -> putStrLn "Invalid option" >> menuLoop

-- | Add a new task.
addTask :: IO ()
addTask = do
  putStr "Title: " >> hFlush stdout
  t <- getLine
  putStr "Description: " >> hFlush stdout
  d <- getLine
  putStr "Due Date (YYYY-MM-DD or leave blank): " >> hFlush stdout
  dateStr <- getLine
  date <- parseDate dateStr
  putStr "Priority (Low | Medium | High): " >> hFlush stdout
  pStr <- getLine
  let prio = fromMaybe Low (readMaybe pStr)
  tasks <- loadTasks
  let newId = nextTaskId tasks
  let task = Task newId t d date prio Pending
  saveTasks (tasks ++ [task])
  putStrLn "Task added."

-- | Display all tasks.
viewTasks :: IO ()
viewTasks = do
  tasks <- loadTasks
  if null tasks
    then putStrLn "No tasks available."
    else mapM_ printTask tasks

-- | Update a task by ID.
updateTask :: IO ()
updateTask = do
  putStr "Enter task ID to update: " >> hFlush stdout
  idStr <- getLine
  case readMaybe idStr of
    Just tid -> do
      tasks <- loadTasks
      putStr "New title: " >> hFlush stdout
      newTitle <- getLine
      let updated = map (updateIfMatch tid newTitle) tasks
      saveTasks updated
      putStrLn "Task updated."
    Nothing -> putStrLn "Invalid ID"

updateIfMatch :: Int -> String -> Task -> Task
updateIfMatch tid newTitle task
  | taskId task /= tid = task
  | otherwise = task { title = newTitle }

-- | Mark a task as completed.
completeTask :: IO ()
completeTask = do
  putStr "Enter task ID to complete: " >> hFlush stdout
  idStr <- getLine
  case readMaybe idStr of
    Just tid -> do
      tasks <- loadTasks
      let updated = map (\t -> if taskId t == tid then t { status = Completed } else t) tasks
      saveTasks updated
      putStrLn "Task marked as completed."
    Nothing -> putStrLn "Invalid ID"

-- | Delete a task by ID.
deleteTask :: IO ()
deleteTask = do
  putStr "Enter task ID to delete: " >> hFlush stdout
  idStr <- getLine
  case readMaybe idStr of
    Just tid -> do
      tasks <- loadTasks
      let updated = filter ((/= tid) . taskId) tasks
      saveTasks updated
      putStrLn "Task deleted."
    Nothing -> putStrLn "Invalid ID"

-- | Display task filtering menu.
filterMenu :: IO ()
filterMenu = do
  putStrLn "Filter by:"
  putStrLn "1. Status"
  putStrLn "2. Priority"
  putStr ">> " >> hFlush stdout
  opt <- getLine
  case opt of
    "1" -> do
      putStr "Enter status (Pending | Completed): " >> hFlush stdout
      s <- getLine
      tasks <- loadTasks
      let result = filter ((== read s) . status) tasks
      mapM_ printTask result
    "2" -> do
      putStr "Enter priority (Low | Medium | High): " >> hFlush stdout
      p <- getLine
      tasks <- loadTasks
      let result = filter ((== read p) . priority) tasks
      mapM_ printTask result
    _   -> putStrLn "Invalid filter option"


-- File: Utils.hs
module Utils where

import Task

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Parse date from string.
parseDate :: String -> IO (Maybe Day)
parseDate "" = return Nothing
parseDate str = return $ parseTimeM True defaultTimeLocale "%Y-%m-%d" str

-- | Pretty print a task.
printTask :: Task -> IO ()
printTask task = do
  putStrLn $ "[" ++ show (taskId task) ++ "] " ++ title task ++ " - " ++ show (status task)
  putStrLn $ "  Description: " ++ description task
  putStrLn $ "  Due Date: " ++ maybe "N/A" show (dueDate task)
  putStrLn $ "  Priority: " ++ show (priority task)

-- | Determine the next available task ID.
nextTaskId :: [Task] -> Int
nextTaskId [] = 1
nextTaskId tasks = 1 + maximum (map taskId tasks)
