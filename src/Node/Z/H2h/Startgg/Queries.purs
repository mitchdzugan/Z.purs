module Node.Z.H2h.Startgg.Queries
  ( IdStub
  , ImagesStub
  , PageInfo
  , PageNode
  , PageNodes
  , PhaseGroupDataRes
  , EventDataRes
  , EventDataVars
  , phaseGroup
  , eventMaxDataPerReq
  , evenMinComplexityPerReq
  ) where

import Z.Prelude

import Node.Z.Gql as Gql
import Node.Z.H2h.Startgg.Queries.PhaseGroupData as PGDQ
import Node.Z.H2h.Startgg.Queries.TourneyData as TDQ
import Node.Z.H2h.Startgg.Queries.TourneyDataSmall as TDSQ

eventMaxDataPerReq :: Gql.Operation EventDataVars EventDataRes
eventMaxDataPerReq = Gql.defOperation TDQ.q Proxy Proxy

evenMinComplexityPerReq :: Gql.Operation EventDataVars EventDataRes
evenMinComplexityPerReq = Gql.defOperation TDSQ.q Proxy Proxy

phaseGroup :: Gql.Operation PhaseGroupDataVars PhaseGroupDataRes
phaseGroup = Gql.defOperation PGDQ.q Proxy Proxy

type PhaseGroupDataVars = { page :: Int, phaseGroupId :: Int }

type EventDataVars = { pageE :: Int, pageS :: Int, slug :: String }

type EventDataRes =
  { event ::
      { id :: Int
      , name :: String
      , slug :: String
      , state :: String
      , tournament ::
          { id :: Int
          , name :: String
          , endAt :: Int
          , images :: ImagesStub
          }
      , standings ::
          PageInfo (placement :: Int, isFinal :: Boolean, entrant :: IdStub)
      , entrants ::
          PageInfo
            ( initialSeedNum :: Maybe Int
            , participants ::
                Array
                  { gamerTag :: String
                  , prefix :: Maybe String
                  , player ::
                      { id :: Int
                      , gamerTag :: String
                      , prefix :: Maybe String
                      , user ::
                          Maybe
                            { genderPronoun :: Maybe String
                            , name :: Maybe String
                            , images :: ImagesStub
                            , authorizations ::
                                Maybe
                                  ( Array
                                      { externalUsername :: Maybe String
                                      , type :: String
                                      }
                                  )
                            }
                      }
                  }
            )
      , phaseGroups ::
          Array
            { id :: Int
            , displayIdentifier :: String
            , phase :: { id :: Int, name :: String, phaseOrder :: Int }
            }
      }
  }

type PhaseGroupDataRes =
  { phaseGroup ::
      { id :: Int
      , bracketType :: String
      , sets ::
          PageInfo
            ( fullRoundText :: String
            , round :: Int
            , wPlacement :: Maybe Int
            , lPlacement :: Maybe Int
            , identifier :: String
            , displayScore :: Maybe String
            , winnerId :: Maybe Int
            , slots :: Array { entrant :: Maybe IdStub }
            , state :: Int
            , games ::
                Maybe
                  ( Array
                      { winnerId :: Maybe Int
                      , orderNum :: Maybe Int
                      , entrant1Score :: Maybe Int
                      , entrant2Score :: Maybe Int
                      , selections ::
                          Maybe
                            ( Array
                                { id :: Int
                                , entrant :: IdStub
                                , orderNum :: Maybe Int
                                , selectionValue :: Int
                                , character :: { id :: Int, name :: String }
                                }
                            )
                      }
                  )
            )
      }
  }

type ImagesStub = Array { url :: Maybe String, type :: String }

type PageNode rest = { id :: Int | rest }

type PageNodes rest = Array (PageNode rest)

type PageInfo rest =
  { pageInfo :: { total :: Int }, nodes :: PageNodes rest }

type IdStub = { id :: Int }
