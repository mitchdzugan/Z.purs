module Node.Z.Git.Client
  ( GitClient
  , GitError(..)
  , git'add
  , git'addAll
  , git'checkout
  , git'client
  , git'client'baseDir
  , git'commit
  , git'commitM
  , git'currentBranch
  , git'isClean
  , git'lastCommitShort
  , git'pull
  , git'push
  , git'revParse
  , git'status
  ) where

import Node.Z.Prelude

import Z.Z.Promise (EffPromise)

foreign import data GitClient :: Type

foreign import js_newClient :: String -> GitClient
foreign import js_clientBaseDir :: GitClient -> String
foreign import js_gitFn
  :: GitClient -> String -> Json -> Array String -> EffPromise String

data GitError
  = GitError'pull String JsError
  | GitError'push String JsError
  | GitError'status String JsError
  | GitError'revParse String JsError
  | GitError'add String JsError
  | GitError'commit String JsError
  | GitError'checkout String JsError

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

err'checkout :: GitClient -> JsError -> GitError
err'checkout c = GitError'checkout $ js_clientBaseDir c

----------------------------

git'client :: forall p. Pathlike p => p -> GitClient
git'client = js_newClient <<< pathStr

git'client'baseDir :: GitClient -> String
git'client'baseDir = js_clientBaseDir

--------------------------------------------

type Git'Fn'g opts res err =
  forall x
   . Generable opts GDefault opts
  => EncodeJson opts
  => (WaS' String opts () @@> Unit)
  -> GitClient
  -> EA' err x @@> res

type Git'Fn opts res = Git'Fn'g opts res GitError

git'fn :: forall @opts. String -> Git'Fn'g opts String JsError
git'fn cmd optsm c = do
  let opts /\ args = sync'x $ w'run $ s'eval default $ optsm *> s'get
  e'runEffPromise $ js_gitFn c cmd (encodeOpts opts) args

git'fn_ :: forall @opts. String -> Git'Fn'g opts Unit JsError
git'fn_ cmd optsm c = git'fn cmd optsm c *> pure unit

-------------------------
type Opts'pull = {}

git'pull :: Git'Fn Opts'pull Unit
git'pull o c = err'pull c !$! git'fn_ "pull" o c

-------------------------
type Opts'push = {}

git'push :: Git'Fn Opts'push Unit
git'push o c = err'push c !$! git'fn_ "push" o c

-------------------------
type Opts'status = { porcelain :: Maybe Boolean }

git'status :: Git'Fn Opts'status String
git'status o c = err'status c !$! git'fn "status" o c

git'isClean :: forall x. GitClient -> EA' GitError x @@> Boolean
git'isClean = map ((==) "") <<< git'status (s'setPure'b @"porcelain" true)

-------------------------
type Opts'revParse = { short :: Maybe Boolean, "abbrev-ref" :: Maybe Boolean }

git'revParse :: Git'Fn Opts'revParse String
git'revParse o c = err'revParse c !$! git'fn "revParse" o c

git'lastCommitShort :: forall x. GitClient -> EA' GitError x @@> String
git'lastCommitShort = git'revParse $ s'setPure'b @"short" true *> w'say "HEAD"

git'currentBranch :: forall x. GitClient -> EA' GitError x @@> String
git'currentBranch =
  git'revParse $ s'setPure'b @"abbrev-ref" true *> w'say "HEAD"

-------------------------
type Opts'add = { all :: Maybe Boolean }

git'add :: Git'Fn Opts'add Unit
git'add o c = err'add c !$! git'fn_ "add" o c

git'addAll :: forall x. GitClient -> EA' GitError x @@> Unit
git'addAll = git'add $ s'setPure'b @"all" true

-------------------------
type Opts'commit = { message :: Maybe String }

git'commit :: Git'Fn Opts'commit Unit
git'commit o c = err'commit c !$! git'fn_ "commit" o c

git'commitM :: forall x. String -> GitClient -> EA' GitError x @@> Unit
git'commitM message = git'commit $ s'setPure'b @"message" message

-------------------------
type Opts'checkout = {}

git'checkout :: Git'Fn Opts'checkout Unit
git'checkout o c = err'checkout c !$! git'fn_ "checkout" o c