module Main exposing (..)

import Html exposing (..)

import Types exposing (..)
import Router as Router
import Navigation exposing (Location)
import Time exposing (Time)
import Json.Decode as JD exposing (field)
-- JS Phoenix Channel based
-- import HttpModule exposing (..)
-- HTTP based
import Debug
import WebData exposing (WebData(..))
import WebData.Http as Http2
import Http

-- Elm Phoenix Channel based
-- import SsPorts exposing (..)
--import Html.CssHelpers exposing (..)
--import Html.CssHelpers exposing (..)
--import HomepageCss exposing (..)
--import SharedStyles exposing (..)
--
--{ id, class, classList } =
--  homepageNamespace
-- import Phoenix.Channel.Helpers exposing (assignResponseType)  --elm 0.18 issue?
--import WebSocket


type alias AppModel =
    { appState : AppState
    , location : Location
    -- , seatsaverModel : SeatSaver.Model
        -- , httpcommentModel : HttpModule.Model
    -- , chatModel : Chat.Model
    , currentTime : Time
    }


type AppState
    = NotReady Time
    | Ready Taco Router.Model

main : Program Flags AppModel Msg 
main =
    -- text "Hello world jason"
    Navigation.programWithFlags UrlChange
        { init = init
        , update = updateWithStorage
        , view = view
        , subscriptions =
            subscriptions
            --[seatListsToSet0, seatsToUpdate0, commentListsToSet0, commentsToUpdate0]
        }


updateWithStorage : Msg -> AppModel -> ( AppModel, Cmd Msg)
updateWithStorage msg model =
    let
        ( newModel, cmds ) = 
            update msg model
    in       
        case model.appState of
            NotReady _ ->
                (newModel, Cmd.none)

            Ready taco routerModel ->
                (newModel
                , Cmd.batch [ cmds])
-- SUBSCRIPTIONS


subscriptions : AppModel -> Sub Msg
subscriptions model =

        Sub.batch
            [ --     seatListsToSet0
            -- , commentListsToSet0
            -- , seatsToUpdate0
            -- , commentsToUpdate0
            -- ,
            (case model.appState of
                NotReady _ ->
                    Sub.none

                Ready _ routerModel ->
                    Router.subscriptions routerModel
                    |> Sub.map RouterMsg)
            -- , (case model.appState of
            --     NotReady _ ->
            --         Sub.none

            --     Ready _ routerModel ->
            --         SeatSaver.subscriptions routerModel.seatsaverModel
            --         |> Sub.map SeatSaverMsg)
            --   Chat.subscriptions model.chatModel
            --     |> Sub.map ChatMsg
            -- , SeatSaver.subscriptions model.seatsaverModel
            --     |> Sub.map SeatSaverMsg
            , Time.every Time.second TimeChange
            ]



-- seatLists broadcast from server
-- MODEL

logDecoder : JD.Decoder Log
logDecoder =
    JD.map2 Log
        (field "id" JD.string)
        (field "title" JD.string)

hnTopStories : String
hnTopStories =
    """
    {
      hn {
        topStories {
          id
          title: id
        }
      }
    }
    """

init : Flags -> Location -> ( AppModel, Cmd Msg )
init flags location =
    let
        --     ( httpModel, httpAct ) =
        --         HttpModule.initialModel
        -- ( ssModel, ssCmd ) =
        --     (SeatSaver.initialModel (Taco flags.currentTime [] flags.userToken flags.username))
         

        -- ( chModel, chatCmd ) =
        --     (Chat.initModel (Taco flags.currentTime [] flags.userToken flags.username))
        _ = Debug.log "inside Main.elm init" (toString flags)
        
        encoded =
            Http.encodeUri hnTopStories

        decoder =
            JD.at [  "data", "hn", "topStories"] <|
                JD.list logDecoder
        
        startModel = {
            appState = NotReady flags.currentTime
          , location = location
        --   , seatsaverModel =
        --         ssModel
                --(SeatSaver.initialModel flags)
                --   , httpcommentModel = httpModel
                --   ,
                --   , chatModel = (Chat.initModel flags)
        --   , chatModel = chModel
          , currentTime = flags.currentTime
          }
        
    in
        (  startModel
        -- (  Maybe.withDefault newLoginModel savedModel
        -- , Cmd.none
         , Http2.get ("https://www.graphqlhub.com/graphql?query=" ++ encoded) HandleLogDataResponse decoder
        -- , Http2.get ("http://localhost:4000/graphql?query=" ++ encoded) HandleLogDataResponse decoder
          -- , Cmd.map HttpModuleMsg httpAct
        --   Cmd.batch
        --     [ Cmd.map SeatSaverMsg ssCmd
        --     , Cmd.map ChatMsg chatCmd
        --     ]
        )



-- UPDATE


type Msg
    --  = SeatSaverMsg SeatSaver.Msg
      -- | HttpModuleMsg HttpModule.Msg
      -- |
    = --ChatMsg Chat.Msg
     HandleLogDataResponse (WebData LogData)
    | UrlChange Location
    | TimeChange Time
    | RouterMsg Router.Msg


update :  Msg -> AppModel -> ( AppModel, Cmd Msg )
update  msg model =

        case msg of
            -- SeatSaverMsg subMsg ->
            --     let
            --         ( updatedSeatSaverModel, cmd, tacoUpdate ) =
            --             SeatSaver.update subMsg model.seatsaverModel
            --     in
            --         ( { model | seatsaverModel = updatedSeatSaverModel }
            --         , Cmd.map SeatSaverMsg cmd
            --         )

            -- HttpModuleMsg subMsg ->
            --     let
            --         ( updatedHttpModel, cmd ) =
            --             HttpModule.update subMsg model.httpcommentModel
            --     in
            --         ( { model | httpcommentModel = updatedHttpModel }
            --         , Cmd.map HttpModuleMsg cmd
            --         )
            -- ChatMsg subMsg ->
            --     let
            --         ( updatedChatModel, cmd, tacoUpdate ) =
            --             Chat.update subMsg model.chatModel
            --     in
            --         ( { model | chatModel = updatedChatModel }
            --         , Cmd.map ChatMsg cmd
            --         )
            -- ChatMsg subMsg ->
            --     case model.appState of
            --         Ready taco routerModel ->
            --         let
            --             ( nextRouterModel, routerCmd, tacoUpdate) =
            --                 Router.update subMsg routerModel
            --         in
            --             ( { model | appState = Ready taco nextRouterModel }
            --             , Cmd.map RouterMsg routerCmd
            --             )

            HandleLogDataResponse webData ->
                updateLogData model webData

            UrlChange location ->
                updateRouter { model | location = location } (Router.UrlChange location)

            RouterMsg routerMsg ->
                updateRouter model routerMsg

            TimeChange time ->
                updateTime model time

updateTime : AppModel -> Time -> ( AppModel, Cmd Msg )
updateTime model time =
    case model.appState of
        NotReady _ ->
            ( { model | appState = NotReady time }
            , Cmd.none
            )

        Ready taco routerModel ->
            ( { model | appState = Ready (updateTaco taco (UpdateTime time)) routerModel }
            , Cmd.none
            )


updateRouter : AppModel -> Router.Msg -> ( AppModel, Cmd Msg )
updateRouter model routerMsg =
    case model.appState of
        Ready taco routerModel ->
            let
                nextTaco =
                    updateTaco taco tacoUpdate

                ( nextRouterModel, routerCmd, tacoUpdate ) =
                    Router.update routerMsg routerModel
            in
                ( { model | appState = Ready nextTaco nextRouterModel }
                , Cmd.map RouterMsg routerCmd
                )

        NotReady _ ->
            Debug.crash "Ooops. We got a sub-component message even though it wasn't supposed to be initialized??"

updateLogData :  AppModel -> WebData LogData -> ( AppModel, Cmd Msg )
updateLogData  model webData =
    case webData of
        Failure _ ->
            Debug.crash "OMG CANT EVEN DOWNLOAD."

        Success logdata ->
            case model.appState of
                NotReady time ->
                    let
                        initTaco =
                            { currentTime = time
                            , logdata = logdata
                      
                            }

                        ( initRouterModel, routerCmd ) =
                            Router.init initTaco model.location
 
                        _ = Debug.log "Main updateLogData Success" (toString routerCmd)

                    in
                        ( { model | appState = Ready initTaco initRouterModel }
                        , Cmd.map RouterMsg routerCmd
                        )

                Ready taco routerModel ->
                    ( { model | appState = Ready (updateTaco taco (UpdateLogData logdata)) routerModel }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )

updateTaco : Taco -> TacoUpdate -> Taco
updateTaco taco tacoUpdate =
    case tacoUpdate of
        UpdateTime time ->
            { taco | currentTime = time }
        
        UpdateLogData logdata ->
            { taco | logdata = logdata}


        NoUpdate ->
            taco



-- VIEW


view : AppModel -> Html Msg
view model =
    case model.appState of
        Ready taco routerModel ->
            Router.view taco routerModel
                |> Html.map RouterMsg

        -- div []
        -- [ Html.map ChatMsg (Chat.view model.chatModel)
        -- , Html.map SeatSaverMsg (SeatSaver.view model.seatsaverModel)
        --   -- , Html.map HttpModuleMsg (HttpModule.view model.httpcommentModel)
        -- ]
        NotReady _ ->
            text "Loading"
