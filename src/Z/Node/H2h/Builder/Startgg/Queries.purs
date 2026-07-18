module Z.Node.H2h.Builder.Startgg.Queries
  ( phaseGroupData
  , tourneyData
  , tourneyDataSmall
  ) where

import Z.Node.Gql.Index as Gql
import Z.Node.H2h.Builder.Startgg.Queries.PhaseGroupData as PGDQ
import Z.Node.H2h.Builder.Startgg.Queries.TourneyData as TDQ
import Z.Node.H2h.Builder.Startgg.Queries.TourneyDataSmall as TDSQ
import Z.Z as Z

tourneyData :: Gql.Operation TourneyDataVars TourneyDataRes
tourneyData = Gql.defOperation TDQ.q Z.Proxy Z.Proxy

tourneyDataSmall :: Gql.Operation TourneyDataVars TourneyDataRes
tourneyDataSmall = Gql.defOperation TDSQ.q Z.Proxy Z.Proxy

phaseGroupData :: Gql.Operation PhaseGroupDataVars PhaseGroupDataRes
phaseGroupData = Gql.defOperation PGDQ.q Z.Proxy Z.Proxy

type PhaseGroupDataVars = { page :: Int, phaseGroupId :: Int }

type TourneyDataVars = { pageE :: Int, pageS :: Int, slug :: String }

type TourneyDataRes =
  { event ::
      { id :: Number
      , name :: String
      , slug :: String
      , state :: String
      , tournament ::
          { id :: Number
          , name :: String
          , endAt :: Number
          , images :: ImagesStub
          }
      , standings ::
          PageNodesRes
            (placement :: Number, isFinal :: Boolean, entrant :: IdStub)
      , entrants ::
          PageNodesRes
            ( initialSeedNum :: Number
            , participants ::
                Array
                  { gamerTag :: String
                  , prefix :: Z.Maybe String
                  , player ::
                      { id :: Number
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
            { id :: Number
            , displayIdentifier :: String
            , phase :: { id :: Number, name :: String, phaseOrder :: Number }
            }
      }
  }

type PhaseGroupDataRes =
  { id :: Number
  , bracketType :: String
  , sets ::
      PageNodesRes
        ( fullRoundText :: String
        , round :: Number
        , wPlacement :: Number
        , lPlacement :: Number
        , identifier :: String
        , displayScore :: Z.Maybe String
        , winnerId :: Z.Maybe Number
        , slots :: Array { entrant :: IdStub }
        , state :: Number
        , games ::
            Z.Maybe
              ( Array
                  { winnerId :: Z.Maybe Number
                  , orderNum :: Number
                  , entrant1Score :: Number
                  , entrant2Score :: Number
                  , selections ::
                      Array
                        { id :: Number
                        , entrant :: IdStub
                        , orderNum :: Number
                        , selectionValue :: String
                        , character :: { id :: Number, name :: String }
                        }
                  }
              )
        )
  }

type ImagesStub = Array { url :: String, type :: String }

type PageNodesRes rest =
  { pageInfo :: { total :: Number }, nodes :: Array { id :: Number | rest } }

type IdStub = { id :: Number }
