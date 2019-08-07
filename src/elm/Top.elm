port module Top exposing (main)

import Array
import Calendar
import Csv
import Csv.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import Random exposing (Seed)
import Task
import Time exposing (Posix)
import UUID exposing (UUID)



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port custDataPort : (String -> msg) -> Sub msg


port accDataPort : (String -> msg) -> Sub msg


port txnDataPort : (String -> msg) -> Sub msg


port userFilePort : String -> Cmd msg


port accountFilePort : String -> Cmd msg


port txFilePort : String -> Cmd msg


port userIdsFilePort : String -> Cmd msg


subscriptions model =
    case model of
        Error _ ->
            Sub.none

        _ ->
            Sub.batch
                [ custDataPort CustData
                , accDataPort AccData
                , txnDataPort TxnData
                ]



-- State Machine


type Model
    = Initial
    | Seeded { seed : Seed }
    | LoadedCust { seed : Seed, customers : List Cust }
    | LoadedAcc { seed : Seed, customers : List Cust, accounts : List Acc }
    | LoadedTxn { seed : Seed, customers : List Cust, accounts : List Acc, txns : List Txn }
    | Error String



-- Events


type Msg
    = CustData String
    | AccData String
    | TxnData String
    | CreateSeed Posix


init _ =
    ( Initial, Task.perform CreateSeed Time.now )



--Time.now )


update msg model =
    let
        ( nextModel, cmd ) =
            updateInner msg model

        _ =
            Debug.log "debug"
                (case nextModel of
                    Initial ->
                        "Initial state."

                    Seeded _ ->
                        "Seeded."

                    LoadedCust { customers } ->
                        "Loaded cust data: " ++ (String.fromInt <| List.length customers) ++ " rows"

                    LoadedAcc { accounts } ->
                        "Loaded acc data: " ++ (String.fromInt <| List.length accounts) ++ " rows"

                    LoadedTxn { txns } ->
                        "Loaded txn data: " ++ (String.fromInt <| List.length txns) ++ " rows"

                    Error err ->
                        err
                )
    in
    ( nextModel, cmd )


updateInner msg model =
    case ( model, msg ) of
        ( Initial, CreateSeed posix ) ->
            ( Seeded { seed = Random.initialSeed <| Time.posixToMillis posix }, Cmd.none )

        ( Seeded { seed }, CustData val ) ->
            let
                csvResult =
                    Csv.parse val
                        |> Result.mapError (always [ "Parse error." ])
                        |> Decode.decode decodeCust
            in
            case csvResult of
                Ok csv ->
                    ( LoadedCust
                        { seed = seed
                        , customers = csv
                        }
                    , Cmd.none
                    )

                Err error ->
                    ( Error "Failed to parse cust data CSV.", Cmd.none )

        ( LoadedCust { seed, customers }, AccData val ) ->
            let
                csvResult =
                    Csv.parse val
                        |> Result.mapError (always [ "Parse error." ])
                        |> Decode.decode decodeAcc
            in
            case csvResult of
                Ok csv ->
                    ( LoadedAcc
                        { seed = seed
                        , customers = customers
                        , accounts = csv
                        }
                    , Cmd.none
                    )

                Err error ->
                    ( Error "Failed to parse acc data CSV.", Cmd.none )

        ( LoadedAcc { seed, customers, accounts }, TxnData val ) ->
            let
                csvResult =
                    Csv.parse val
                        |> Result.mapError (always [ "Parse error." ])
                        |> Decode.decode decodeTxn
            in
            case csvResult of
                Ok csv ->
                    let
                        ( usersByCin, seed1 ) =
                            processCustomers customers seed

                        ( accountsByCin, cinsByAccountId, seed2 ) =
                            processAccounts accounts seed1

                        usersByAccountId =
                            Dict.foldl
                                (\accountId cin accum ->
                                    let
                                        maybeUser =
                                            Dict.get cin usersByCin
                                    in
                                    case maybeUser of
                                        Nothing ->
                                            accum

                                        Just user ->
                                            Dict.insert accountId user accum
                                )
                                Dict.empty
                                cinsByAccountId

                        ( transactionsBatch, seed3 ) =
                            processTransactions usersByAccountId csv seed2

                        userData =
                            Dict.values usersByCin
                                |> List.map (encodeUserEntity >> Encode.encode 0)
                                |> List.intersperse "\n"
                                |> String.concat

                        accountData =
                            Dict.toList accountsByCin
                                |> List.map (Tuple.mapSecond (AccountListEntity >> encodeAccountListEntity >> Encode.encode 0))
                                |> List.map (\( k, v ) -> k ++ " " ++ v)
                                |> List.intersperse "\n"
                                |> String.concat

                        txnData =
                            transactionsBatch
                                |> encodeCreateTransactionsBatch
                                |> Encode.encode 3

                        userIdData =
                            Dict.keys usersByCin
                                |> List.intersperse "\n"
                                |> String.concat
                    in
                    ( LoadedTxn
                        { seed = seed
                        , customers = customers
                        , accounts = accounts
                        , txns = csv
                        }
                    , Cmd.batch
                        [ txFilePort txnData
                        , accountFilePort accountData
                        , userFilePort userData
                        , userIdsFilePort userIdData
                        ]
                    )

                Err error ->
                    ( Error "Failed to parse txn data CSV."
                    , Cmd.none
                    )

        ( _, _ ) ->
            ( model, Cmd.none )



-- CSV Processing into Tink Data Format


processCustomers : List Cust -> Seed -> ( Dict String UserEntity, Seed )
processCustomers customers seed =
    List.foldl
        (\cust ( accum, currSeed ) ->
            let
                ( user, nextSeed ) =
                    custToTink cust currSeed
            in
            ( Dict.insert cust.cin user accum, nextSeed )
        )
        ( Dict.empty, seed )
        customers


processAccounts : List Acc -> Seed -> ( Dict String (List AccountEntity), Dict String String, Seed )
processAccounts accounts seed =
    List.foldl
        (\acc ( accountsByCin, cinsByAccountId, currSeed ) ->
            let
                ( account, nextSeed ) =
                    accToTink acc currSeed
            in
            ( Dict.update acc.cin
                (\maybeList ->
                    case maybeList of
                        Nothing ->
                            Just [ account ]

                        Just entities ->
                            Just <| account :: entities
                )
                accountsByCin
            , Dict.insert (acc.sortCode ++ acc.accountNo) acc.cin cinsByAccountId
            , nextSeed
            )
        )
        ( Dict.empty
        , Dict.empty
        , seed
        )
        accounts


processTransactions : Dict String UserEntity -> List Txn -> Seed -> ( CreateTransactionBatch, Seed )
processTransactions usersByAccountId transactions seed =
    let
        groupedTransactions =
            groupTransactions usersByAccountId transactions

        ( entityId, seed1 ) =
            randUuid seed

        ( entities, newSeed ) =
            Dict.foldl
                (\userId accountList ( accum, currSeed ) ->
                    let
                        ( transactionAccounts, nextSeed ) =
                            accountListToCreateAccountEntities accountList currSeed
                    in
                    ( { entityId = entityId
                      , externalUserId = userId
                      , container =
                            { type_ = "HISTORICAL"
                            , transactionAccounts = transactionAccounts
                            }
                      }
                        :: accum
                    , nextSeed
                    )
                )
                ( [], seed )
                groupedTransactions
    in
    ( { ingestEntities = entities }, newSeed )


accountListToCreateAccountEntities : List ( String, List Txn ) -> Seed -> ( List CreateTransactionAccountEntity, Seed )
accountListToCreateAccountEntities accountList seed =
    List.foldl
        (\accountWithTxs ( accum, currSeed ) ->
            let
                ( entity, nextSeed ) =
                    txnsToTransactionAccountEntity accountWithTxs currSeed
            in
            ( entity :: accum, nextSeed )
        )
        ( [], seed )
        accountList


txnsToTransactionAccountEntity : ( String, List Txn ) -> Seed -> ( CreateTransactionAccountEntity, Seed )
txnsToTransactionAccountEntity ( accountId, transactions ) seed =
    let
        ( txEntities, newSeed ) =
            List.foldl
                (\txn ( accum, currSeed ) ->
                    let
                        ( entity, nextSeed ) =
                            txnToCreateTransactionEntity txn currSeed
                    in
                    ( entity :: accum, nextSeed )
                )
                ( [], seed )
                transactions
    in
    ( { balance = 100.0
      , externalId = accountId
      , transactions = txEntities
      }
    , newSeed
    )


txnToCreateTransactionEntity : Txn -> Seed -> ( CreateTransactionEntity, Seed )
txnToCreateTransactionEntity txn seed =
    let
        ( externalId, seed1 ) =
            randUuid seed

        sanitizeDescription desc =
            if String.trim desc == "" then
                "EMPTY DESC"

            else
                String.trim desc
    in
    ( { amount = txn.amount
      , date = dateToMillis txn.postedDate
      , description = sanitizeDescription txn.description
      , externalId = externalId
      , type_ = "PAYMENT"
      }
    , seed1
    )


{-| Group the transactions by user and then by account.
-}
groupTransactions : Dict String UserEntity -> List Txn -> Dict String (List ( String, List Txn ))
groupTransactions usersByAccountId transactions =
    groupTransactionsByAcountId transactions
        |> groupAccountsByUser usersByAccountId


groupTransactionsByAcountId : List Txn -> Dict String (List Txn)
groupTransactionsByAcountId transactions =
    List.foldl
        (\txn accum ->
            Dict.update txn.accountId
                (\maybeTxns ->
                    case maybeTxns of
                        Nothing ->
                            Just [ txn ]

                        Just txns ->
                            Just <| txn :: txns
                )
                accum
        )
        Dict.empty
        transactions


groupAccountsByUser : Dict String UserEntity -> Dict String (List Txn) -> Dict String (List ( String, List Txn ))
groupAccountsByUser usersByAccountId txnsByAccountId =
    Dict.foldl
        (\accountId txns accum ->
            let
                maybeUser =
                    Dict.get accountId usersByAccountId
            in
            case maybeUser of
                Nothing ->
                    let
                        _ =
                            Debug.log "could not find the user for the account"
                    in
                    accum

                Just user ->
                    Dict.update user.externalId
                        (\maybeTxnList ->
                            case maybeTxnList of
                                Nothing ->
                                    Just [ ( accountId, txns ) ]

                                Just txnList ->
                                    Just <| ( accountId, txns ) :: txnList
                        )
                        accum
        )
        Dict.empty
        txnsByAccountId


custToTink : Cust -> Seed -> ( UserEntity, Seed )
custToTink cust seed =
    let
        ( externalId, seed1 ) =
            randUuid seed

        ( token, seed2 ) =
            randUuid seed1
    in
    ( { externalId = cust.cin
      , token = token
      , market = "UK"
      , locale = "en_GB"
      }
    , seed2
    )


accToTink : Acc -> Seed -> ( AccountEntity, Seed )
accToTink acc seed =
    let
        ( externalId, seed1 ) =
            randUuid seed
    in
    ( { balance =
            String.toFloat acc.balance
                |> Maybe.withDefault 0
                |> String.fromFloat
      , externalId = acc.sortCode ++ acc.accountNo
      , name = "RANDOM NAME"
      , number = acc.accountNo
      , type_ = "CHECKING"
      }
    , seed1
    )



-- CSV Decoders


type alias Cust =
    { cin : String }


type alias Acc =
    { cin : String
    , sortCode : String
    , accountNo : String
    , balance : String
    }


type alias Txn =
    { postedDate : String
    , accountId : String
    , currency : String
    , amount : String
    , txnType : String
    , description : String
    }


decodeCust : Decoder (Cust -> a) a
decodeCust =
    Decode.map Cust
        (Decode.field "CIN" Ok)


decodeAcc : Decoder (Acc -> a) a
decodeAcc =
    Decode.map Acc
        (Decode.field "CIN" Ok
            |> Decode.andMap (Decode.field "AGRMNT_BRNCH_NUM" Ok)
            |> Decode.andMap (Decode.field "AGRMNT_HOST_NUM" Ok)
            |> Decode.andMap (Decode.field "EOD_CLRD_BAL_FOR_FTE_AMT" Ok)
        )


decodeTxn : Decoder (Txn -> a) a
decodeTxn =
    Decode.map Txn
        (Decode.field "FIN_EVNT_PSTD_DT" Ok
            |> Decode.andMap (Decode.field "AGRMNT_ID_TRN_ACCT" Ok)
            |> Decode.andMap (Decode.field "FIN_EVNT_STLMNT_CURY_CD" Ok)
            |> Decode.andMap (Decode.field "FIN_EVNT_STLMNT_AMT" Ok)
            |> Decode.andMap (Decode.field "SRC_TRX_TYP_CD" Ok)
            |> Decode.andMap (Decode.field "EVNT_TXT" Ok)
        )



-- Tink API Models.


type alias UserEntity =
    { externalId : String
    , token : String
    , market : String
    , locale : String
    }


encodeUserEntity : UserEntity -> Value
encodeUserEntity user =
    [ ( "externalId", Encode.string user.externalId )
    , ( "token", Encode.string user.token )
    , ( "market", Encode.string user.market )
    , ( "locale", Encode.string user.locale )
    ]
        |> Encode.object


type alias AccountListEntity =
    { accounts : List AccountEntity
    }


encodeAccountListEntity : AccountListEntity -> Value
encodeAccountListEntity accountList =
    [ ( "accounts", Encode.list encodeAccountEntity accountList.accounts ) ]
        |> Encode.object


type alias AccountEntity =
    { balance : String
    , externalId : String
    , name : String
    , number : String
    , type_ : String
    }


encodeAccountEntity : AccountEntity -> Value
encodeAccountEntity account =
    [ ( "balance", Encode.string account.balance )
    , ( "externalId", Encode.string account.externalId )
    , ( "name", Encode.string account.name )
    , ( "number", Encode.string account.number )
    , ( "type", Encode.string account.type_ )
    ]
        |> Encode.object


type alias CreateTransactionBatch =
    { ingestEntities : List IngestTransactionEntity
    }


encodeCreateTransactionsBatch : CreateTransactionBatch -> Value
encodeCreateTransactionsBatch batch =
    [ ( "ingestEntities", Encode.list encodeIngestTransactionEntity batch.ingestEntities )
    ]
        |> Encode.object


type alias IngestTransactionEntity =
    { entityId : String
    , externalUserId : String
    , container : CreateTransactionAccountContainer
    }


encodeIngestTransactionEntity : IngestTransactionEntity -> Value
encodeIngestTransactionEntity entity =
    [ ( "entityId", Encode.string entity.entityId )
    , ( "externalUserId", Encode.string entity.externalUserId )
    , ( "container", encodeCreateTransactionAccountContainer entity.container )
    ]
        |> Encode.object


type alias CreateTransactionAccountContainer =
    { type_ : String
    , transactionAccounts : List CreateTransactionAccountEntity
    }


encodeCreateTransactionAccountContainer : CreateTransactionAccountContainer -> Value
encodeCreateTransactionAccountContainer container =
    [ ( "type", Encode.string container.type_ )
    , ( "transactionAccounts", Encode.list encodeCreateTransactionAccountEntity container.transactionAccounts )
    ]
        |> Encode.object


type alias CreateTransactionAccountEntity =
    { balance : Float
    , externalId : String
    , transactions : List CreateTransactionEntity
    }


encodeCreateTransactionAccountEntity : CreateTransactionAccountEntity -> Value
encodeCreateTransactionAccountEntity entity =
    [ ( "balance", Encode.float entity.balance )
    , ( "externalId", Encode.string entity.externalId )
    , ( "transactions", Encode.list encodeCreateTransactionEntity entity.transactions )
    ]
        |> Encode.object


type alias CreateTransactionEntity =
    { amount : String
    , date : Int
    , description : String
    , externalId : String
    , type_ : String
    }


encodeCreateTransactionEntity : CreateTransactionEntity -> Value
encodeCreateTransactionEntity entity =
    [ ( "amount", Encode.string entity.amount )
    , ( "date", Encode.int entity.date )
    , ( "description", Encode.string entity.description )
    , ( "externalId", Encode.string entity.externalId )
    , ( "type", Encode.string entity.type_ )
    ]
        |> Encode.object



-- Helper functions


randUuid : Seed -> ( String, Seed )
randUuid seed =
    Random.step UUID.generator seed
        |> Tuple.mapFirst UUID.canonical


dateToMillis : String -> Int
dateToMillis val =
    let
        ymd =
            String.split "-" val
                |> Array.fromList

        getMonth m =
            Array.get m Calendar.months
    in
    { day =
        Array.get 2 ymd
            |> Maybe.andThen String.toInt
            |> Maybe.withDefault 1
    , month =
        Array.get 1 ymd
            |> Maybe.andThen String.toInt
            |> Maybe.andThen getMonth
            |> Maybe.withDefault Time.Jan
    , year =
        Array.get 0 ymd
            |> Maybe.andThen String.toInt
            |> Maybe.withDefault 2000
    }
        |> Calendar.fromRawParts
        |> Maybe.map Calendar.toMillis
        |> Maybe.withDefault 0
