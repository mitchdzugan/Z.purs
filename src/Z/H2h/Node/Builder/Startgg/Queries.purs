module Z.H2h.Node.Builder.Startgg.Queries
  ( IdStub
  , ImagesStub
  , PageInfo
  , PageNode
  , PageNodes
  , PhaseGroupDataRes
  , EventDataRes
  , EventDataVars
  , phaseGroup
  , event
  , eventSmall
  ) where

import Z.Gql.Node.Module as Gql
import Z.H2h.Node.Builder.Startgg.Queries.PhaseGroupData as PGDQ
import Z.H2h.Node.Builder.Startgg.Queries.TourneyData as TDQ
import Z.H2h.Node.Builder.Startgg.Queries.TourneyDataSmall as TDSQ
import Z as Z

event :: Gql.Operation EventDataVars EventDataRes
event = Gql.defOperation TDQ.q Z.Proxy Z.Proxy

eventSmall :: Gql.Operation EventDataVars EventDataRes
eventSmall = Gql.defOperation TDSQ.q Z.Proxy Z.Proxy

phaseGroup :: Gql.Operation PhaseGroupDataVars PhaseGroupDataRes
phaseGroup = Gql.defOperation PGDQ.q Z.Proxy Z.Proxy

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
          PageInfo
            (placement :: Int, isFinal :: Boolean, entrant :: IdStub)
      , entrants ::
          PageInfo
            ( initialSeedNum :: Int
            , participants ::
                Array
                  { gamerTag :: String
                  , prefix :: Z.Maybe String
                  , player ::
                      { id :: Int
                      , gamerTag :: String
                      , prefix :: Z.Maybe String
                      , user ::
                          Z.Maybe
                            { genderPronoun :: Z.Maybe String
                            , name :: Z.Maybe String
                            , images :: ImagesStub
                            , authorizations ::
                                Z.Maybe
                                  ( Array
                                      { externalUsername :: String
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
            , wPlacement :: Z.Maybe Int
            , lPlacement :: Z.Maybe Int
            , identifier :: String
            , displayScore :: Z.Maybe String
            , winnerId :: Z.Maybe Int
            , slots :: Array { entrant :: IdStub }
            , state :: Int
            , games ::
                Z.Maybe
                  ( Array
                      { winnerId :: Z.Maybe Int
                      , orderNum :: Z.Maybe Int
                      , entrant1Score :: Int
                      , entrant2Score :: Int
                      , selections ::
                          Z.Maybe
                            ( Array
                                { id :: Int
                                , entrant :: IdStub
                                , orderNum :: Z.Maybe Int
                                , selectionValue :: Int
                                , character :: { id :: Int, name :: String }
                                }
                            )
                      }
                  )
            )
      }
  }

type ImagesStub = Array { url :: String, type :: String }

type PageNode rest = { id :: Int | rest }

type PageNodes rest = Array (PageNode rest)

type PageInfo rest =
  { pageInfo :: { total :: Int }, nodes :: PageNodes rest }

type IdStub = { id :: Int }
