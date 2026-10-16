module Node.Z.Git.Client
  ( GitClient
  , GitError(..)
  , git'add
  , git'client
  , git'client'baseDir
  , git'commit
  , git'isClean
  , git'lastCommit
  , git'lastCommitShort
  , git'pull
  , git'push
  , git'revParse
  , git'status
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Node.Z.Sys.SysImpl (class Pathlike, pathStr)
import Z.Z.Core (EffPromise, JsError)
import Z.Z.X.Index (type (@@>), EA', e'runEffA, e'runEffPromise, (!$!))

foreign import data GitClient :: Type

foreign import js_clientBaseDir :: GitClient -> String
foreign import js_newClient :: String -> GitClient
foreign import js_pull :: GitClient -> EffPromise Unit
foreign import js_push :: GitClient -> EffPromise Unit
foreign import js_status :: GitClient -> Boolean -> EffPromise String
foreign import js_revParse
  :: GitClient -> Boolean -> String -> EffPromise String

foreign import js_add :: GitClient -> Boolean -> EffPromise Unit
foreign import js_commit :: GitClient -> String -> EffPromise Unit

data GitError
  = GitError'pull String JsError
  | GitError'push String JsError
  | GitError'status String JsError
  | GitError'revParse String JsError
  | GitError'add String JsError
  | GitError'commit String JsError

derive instance Generic GitError _
instance EncodeJson GitError where
  encodeJson = genericEncodeJson

err'pull :: GitClient -> JsError -> GitError
err'pull c = GitError'pull $ js_clientBaseDir c

err'push :: GitClient -> JsError -> GitError
err'push c = GitError'push $ js_clientBaseDir c

err'status :: GitClient -> JsError -> GitError
err'status c = GitError'status $ js_clientBaseDir c

err'revParse :: GitClient -> JsError -> GitError
err'revParse c = GitError'revParse $ js_clientBaseDir c

err'add :: GitClient -> JsError -> GitError
err'add c = GitError'add $ js_clientBaseDir c

err'commit :: GitClient -> JsError -> GitError
err'commit c = GitError'commit $ js_clientBaseDir c

----------------------------

git'client'baseDir :: GitClient -> String
git'client'baseDir = js_clientBaseDir

git'client :: forall p. Pathlike p => p -> GitClient
git'client = js_newClient <<< pathStr

git'pull :: forall x. GitClient -> EA' GitError x @@> Unit
git'pull c = err'pull c !$! e'runEffPromise $ js_pull c

git'push :: forall x. GitClient -> EA' GitError x @@> Unit
git'push c = err'push c !$! e'runEffPromise $ js_push c

git'status :: forall x. GitClient -> Boolean -> EA' GitError x @@> String
git'status c porcelain =
  err'status c !$! e'runEffPromise $ js_status c porcelain

git'isClean :: forall x. GitClient -> EA' GitError x @@> Boolean
git'isClean c = git'status c true <#> (==) ""

git'revParse
  :: forall x. GitClient -> Boolean -> String -> EA' GitError x @@> String
git'revParse c short commit =
  err'revParse c !$! e'runEffPromise $ js_revParse c short commit

git'lastCommit :: forall x. GitClient -> Boolean -> EA' GitError x @@> String
git'lastCommit c short = git'revParse c short "HEAD"

git'lastCommitShort :: forall x. GitClient -> EA' GitError x @@> String
git'lastCommitShort c = git'lastCommit c true

git'add :: forall x. GitClient -> Boolean -> EA' GitError x @@> Unit
git'add c all = err'add c !$! e'runEffPromise $ js_add c all

git'commit :: forall x. GitClient -> String -> EA' GitError x @@> Unit
git'commit c message = err'commit c !$! e'runEffPromise $ js_commit c message