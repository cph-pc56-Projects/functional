{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics
import Prelude hiding (id)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent( newMVar
                         , readMVar
                         , takeMVar
                         , putMVar
                         , MVar
                         )
import Control.Monad.Trans (lift)
import System.IO.Unsafe (unsafePerformIO)
import Data.Text.Lazy as T


data Member = Member
  { id :: Int
  , name :: String
  , email :: String
  } deriving (Show, Generic)
instance ToJSON Member
instance FromJSON Member

data Error = Error String deriving (Show, Generic)
instance ToJSON Error


membersRef :: MVar (IntMap Member)
membersRef = unsafePerformIO $ newMVar (IntMap.fromList [(1, Member 1 "Name" "Email")])

insertMember :: Member -> IntMap Member -> (Member, IntMap Member)
insertMember member map
  | IntMap.member (id member) map = (member , IntMap.insert (id member) member map)
  | otherwise = do
      let newId = IntMap.size map + 1
      let newMember = Member newId (name member) (email member)
      (newMember, IntMap.insert newId newMember map)


main :: IO ()
main = do
  scotty 3003 $ do
    middleware simpleCors
    get "/member/count" $ do
      members <- lift $ readMVar membersRef
      text $ T.pack (show (IntMap.size members))
    get "/member" $ do
      members <- lift $ readMVar membersRef
      json (IntMap.elems members)
    get "/member/:id" $ do
        members <- lift $ readMVar membersRef
        id <- param "id"
        case IntMap.lookup id members of
          Just member -> json member
          Nothing -> json $ Error "No member with this id"
    post "/member" $ do
        payload <- body
        case decode payload of
          Just member -> do
            members <- lift $ takeMVar membersRef
            let (newMember, newMembersMap) = insertMember member members
            lift $ putMVar membersRef newMembersMap
            json newMember
          Nothing -> json $ Error "Wrong payload format"
