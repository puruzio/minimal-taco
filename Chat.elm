module Chat exposing (..)

import Html exposing (Html, header, div, nav, h4, img, text, ul, li, input, form, button, br, table, tbody, tr, td, span)
import Html.Attributes exposing (style, type_, src, value, placeholder)
import Html.Events exposing (onInput, onSubmit, onClick)
import Http
import Platform.Cmd
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Dict
import Debug
import Date.Extra as Dt
import Task exposing (Task)
import Material
import Material.Scheme
import Types exposing (Taco, Log,  TacoUpdate(..))

-- import Material.Button as Button

import Material.Chip as Chip
import Material.Toggles as Toggles
import Material.Options as Options exposing (css)
import Material.Color as Color
import Material.List as Lists


type alias User =
    { name : String
    }


type alias UserPresence =
    { online_at : String
    , device : String
    , name : String
    }




send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


logDecoder : JD.Decoder Log
logDecoder =
    JD.map3 Log
        (field "id" JD.string)
        (field "text" JD.string)
        (field "created_at" JD.string)


hnTopStories : String
hnTopStories =
    """\x0D
    {\x0D
        logs {\x0D
          id\x0D
          text\x0D
          created_at\x0D
      }\x0D
    }\x0D
    """


request : Http.Request (List Log)
request =
    let
        encoded =
            Http.encodeUri hnTopStories

        decoder =
            JD.at [ "data", "logs" ] <|
                JD.list logDecoder
    in
        -- port 3001 is currently pointing to old\finreporting which is alltranslog
        Http.get ("http://lwvpweact001:4000/graphql?query=" ++ encoded) decoder



-- MODEL


type Msg
    = 
     FetchHNTopStories (Result Http.Error (List Log))
      -- ReceiveMessage String

      -- | ReceivePresenceMessage JE.Value
    -- | ShowJoinedMessage String
    -- | ShowLeftMessage String
      -- | ConnectSocket
      --  | SetUsername String
    -- | HandlePresenceState JE.Value
    -- | HandlePresenceDiff JE.Value
    | ChannelJoinToggle
    | Mdl (Material.Msg Msg)
    | Increase
      -- | Heartbeat Time
    | NoOp


type alias Model =
    { newMessage : String

        -- , presence : List String
    , users : List User
    , mdl : Material.Model
    , count : Int
    , if_channeljoined : Bool
    , response : String
    , logs : List Log
    }


type alias Mdl =
    Material.Model




-- This is where the ChatMessage from Phoenix BE comes in through Socket


initModel : Taco -> ( Model, Cmd Msg )
initModel taco =
    let
        startModel = { newMessage = ""
                        -- , presence = []
                    , users = []
                    , mdl = Material.model
                    , count = 0
                    , if_channeljoined = False
                    , response = "Waiting for a response..."
                    , logs = [] --taco.logdata  -- from localstorage
                    }


        cmd =  Http.send FetchHNTopStories request
        -- cmd = send (ChannelJoinToggle startModel) -- Http.send FetchHNTopStories request
            -- if taco.chatChannel /= "" then
                -- Cmd.batch
                --         [Http.send FetchHNTopStories request
                --         , (send (JoinChannel startModel)) ]
            -- else
            --     Cmd.batch
            --             [Http.send FetchHNTopStories request
            --                 ]           

        _ = Debug.log "Chat initmodel chatModel : " (toString cmd)

    in
    -- Model "" [] [] flags.username (initPhxSocket flags)
        ( startModel
           ! [cmd]
        )



-- init : ( Flags -> Model, Cmd Msg )
-- init =
--     -- ( initModel, Cmd.none )
--     ( initModel ! [Http.send FetchHNTopStories request ] )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none




-- PHOENIX STUFF


type alias ChatMessage =
    { username : String
    , body : String
    , timestamp : String
    }


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.map3 ChatMessage
        (JD.oneOf
            [ (field "username" JD.string)
            , JD.succeed "anonymous"
            ]
        )
        (field "body" JD.string)
        (field "timestamp" JD.string)



-- UPDATE



-- JE.object [ ("username", JE.string "123") ]


update : Msg -> Model -> ( Model, Cmd Msg, TacoUpdate )
update msg model =
    case msg of
        -- ReceiveMessage str ->
        --     ( { model | messages = str :: model.messages }
        --     , Cmd.none
        --     )
        FetchHNTopStories (Ok logs) ->
            ({ model | logs = logs, response = "Alltrans Log>>" } 
            , Cmd.none
            , NoUpdate
             )

        FetchHNTopStories (Err res) ->
            ({ model | response = toString res }
            , Cmd.none
            , NoUpdate)



        ChannelJoinToggle ->
            let
                _ = Debug.log "test" "print"
            in
                ( model, Cmd.none, NoUpdate )

        Mdl msg_ ->
            let 
                (mdl, command) = Material.update Mdl msg_ model
            in    
                (mdl, command, NoUpdate)

        Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none, NoUpdate
            )

        NoOp ->
            ( model, Cmd.none, NoUpdate )



-- { id, class, classList } =
--     homepageNamespace
-- VIEW

listItem : Log -> Html Msg
listItem log =
    Lists.li [Lists.withSubtitle]
        [ Lists.content []
          [ text (log.text) 
        
            , Lists.subtitle [] [ text (log.created_at) ]
          ]
        ]

view :Taco -> Model -> Html Msg
view taco model =
    let
        -- items = List.map (listItem << .text ) model.logs
        items =
            List.map listItem model.logs

        logList =
            Lists.ul [ css "margin" "0", css "padding" "0" ] items
    in
        div [ style [ ( "border", "solid" ) ] ]
            [ --     header []
              --     [ img [ id ReactiveLogo, src "assets/logo.png" ] []
              --     , nav []
              --         navElems
              --     , span [ id BuyTickets ]
              --         [ text "BUY TICKETS" ]
              --     ]
              -- , div []
              --     [ img [ src "assets/banner.png" ] [] ]
              div [] [ logList ]
            , h4 [] [ text "Channels:" ]
              -- , div
              --     []
              --     [ Button.render Mdl [ 0 ] model.mdl [ Button.raised, Button.colored, Options.onClick (JoinChannel model) ] [ text "Join channel" ]
              --     , Button.render Mdl [ 1 ] model.mdl [ Button.raised, Button.colored, Options.onClick LeaveChannel model ] [ text "Leave channel" ]
              --     ]
            , Toggles.switch Mdl [ 0 ] model.mdl [ Options.onToggle (ChannelJoinToggle), Toggles.ripple, Toggles.value model.if_channeljoined ] [ text "Join" ]
              --model.if_channeljoined
              -- [ button [ onClick (JoinChannel model) ] [ text "Join channel" ]
              -- , button [ onClick LeaveChannel ] [ text "Leave channel" ]
              -- ]
            --, channelsTable (Dict.values model.phxSocket.channels)
          
            , br [] []
            , div [] [ h4 [] [ text "Presence users:" ] ]
            , ul [] ((List.reverse << List.map renderUser) model.users)
            , br [] []
            
            , h4 [] [ text "Message:" ]
            -- , newMessageForm model
          
              -- , div [] [ chatInterfaceView model]
            ]
            |> Material.Scheme.top




-- newMessageForm : Model -> Html Msg
-- newMessageForm model =
--     form [ onSubmit SendMessage ]
--         [ input [ style [ ( "width", "50%" ) ], type_ "text", value model.newMessage, onInput SetNewMessage, placeholder "Type your message" ] []
--         ]


renderMessage : String -> Html Msg
renderMessage str =
    li [] [ text str ]


renderUser : User -> Html Msg
renderUser user =
    -- li [] [ text user.name ]
    -- li []
    --     [
    Chip.span []
        [ Chip.contact Html.span
            [ Color.background Color.primary
            , Color.text Color.white
            ]
            [ text (user.name |> String.toUpper |> String.slice 0 1) ]
        , Chip.content []
            [ text user.name ]
        ]



-- ]
-------------------
----Styles
-----------------


boldStyle : List ( String, String )
boldStyle =
    [ ( "font-weight", "bold" ) ]


buildStyle : List (List ( String, String )) -> Html.Attribute msg
buildStyle styleLists =
    Html.Attributes.style <| List.concat styleLists

