module Z.CLM.Stats.P
  ( p'buildOutputs
  , p'clmRanks
  , p'defaultSeasonId
  , p'editRoute
  , p'nextClmId
  , p'participants
  , p'ratingData
  , p'route
  , p'setGroups
  ) where

import Z.Prelude

p'defaultSeasonId :: Proxy "defaultSeasonId"
p'defaultSeasonId = default

p'nextClmId :: Proxy "nextClmId"
p'nextClmId = default

p'setGroups :: Proxy "setGroups"
p'setGroups = default

p'participants :: Proxy "participants"
p'participants = default

p'ratingData :: Proxy "ratingData"
p'ratingData = default

p'clmRanks :: Proxy "clmRanks"
p'clmRanks = default

p'route :: Proxy "route"
p'route = default

p'editRoute :: Proxy "editRoute"
p'editRoute = default

p'buildOutputs :: Proxy "buildOutputs"
p'buildOutputs = default