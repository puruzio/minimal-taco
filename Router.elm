module Router exposing (..)

import Navigation exposing (Location)
import Styles exposing (..)
import Html exposing (..)
-- import Html.Attributes exposing (href)
import Html.Events exposing (..)

import Types exposing (TacoUpdate(..), Taco)
import Helpers exposing (Route(..), parseLocation, reverseRoute)
import Chat as Chat

type alias Model =
    { chatModel : Chat.Model
      , route: Route}

type  Msg
  = UrlChange Location
  | NavigateTo Route
  | ChatMsg Chat.Msg

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Chat.subscriptions model.chatModel
        |> Sub.map ChatMsg

    ]
    
init : Taco -> Location -> (Model, Cmd Msg)
init taco location =
    let 
        ( chatModel, chatCmd) =
        Chat.initModel taco

        _ = Debug.log "chatCmd in Router init" (toString chatCmd)



    in
      ( { chatModel = chatModel
      , route = parseLocation location}
      
      , Cmd.batch 
        [
             Cmd.map ChatMsg chatCmd
        ] 
      )

update: Msg -> Model -> ( Model,Cmd Msg, TacoUpdate)
update msg model =
    case msg of
        UrlChange location ->
            ({model |route = parseLocation location}
            , Cmd.none
            , NoUpdate
            )

        NavigateTo route ->
            ( model
            , Navigation.newUrl (reverseRoute route)
            , NoUpdate
            )

        ChatMsg chatMsg ->
            updateChat model chatMsg


updateChat : Model -> Chat.Msg -> (Model, Cmd Msg, TacoUpdate)
updateChat model chatMsg =
    let
        ( nextChatModel, chatCmd, tacoUpdate) =
            Chat.update chatMsg model.chatModel
    in
        ( {model | chatModel = nextChatModel }
        , Cmd.map ChatMsg chatCmd
        , tacoUpdate)



view : Taco -> Model -> Html Msg
view taco model = 
    let     
        buttonStyles route = 
            if model.route == route then
                styles navigationButtonActive
            else
                styles navigationButton
    in
        div [ styles ( appStyles ++ wrapper)]
            [ header [ styles headerSection]
                [ p [] [text ("")]
                ]
                , 
                nav [styles navigationBar ]
                 [ button
                    [onClick (NavigateTo ChatRoute)
                    , buttonStyles ChatRoute
                    ]
                    [ text ("AlltransLog")]

                   
                ]
                , pageView taco model
                 
            ]

pageView : Taco -> Model -> Html Msg
pageView taco model =
    div [styles activeView]
        [(case model.route of
            ChatRoute ->
                Chat.view taco model.chatModel
                    |> Html.map ChatMsg

            NotFoundRoute ->
                h1 [] [ text "404 :("]
        )]