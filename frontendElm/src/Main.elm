module Main exposing (..)

import Browser
import Browser.Events
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html
import Http exposing (Error(..), emptyBody, header, jsonBody, stringBody)
import Json.Decode as JD
import Json.Encode as JE
import Svg as S
import Svg.Attributes as SA


type alias Model =
    { iteamTitle : String
    , iteamThumbnail : String
    , iteamPages : Int 
    , iteamLink : String
    , iteamPublisher : String
    , iteamTopicId : Int
    , iteamId : Int 
    , poruka : String
    , results : List Iteam
    , resultIteam : Maybe Iteam 
    , errorMessage : Maybe String
    , loading : Bool
    }


type alias Iteam =
    { title : String
    , thumbnail : Maybe String
    , link : String
    , pages : Maybe Int
    , publisher : Maybe String
    }


type Msg
    = MsgGetIteams
    | MsgGetIteamsEnviroment
    | MsgGetIteamsHealth
    | MsgGetIteamsSport
    | MsgGetIteamsTechnology
    | MsgGetIteamsWorld
    | MsgGetIteamsLargest
    | MsgGotResults (Result Http.Error (List Iteam))
    | MsgSuccesfulPost (Result Http.Error ()) --(Result Http.Error String) --TODO DODAJ S
      --| GotText (Result Http.Error String)
    | MsgInputTitleField String
    | MsgInputThumbnailField String
    | MsgInputPagesFieldAsString String
    | MsgInputLinkField String
    | MsgInputPublisherField String
    | MsgInputTopicIdFieldAsString String
    | MsgAddIteam
    | MsgDeleteIteam
    | MsgShowIteam
    | MsgInputIdFieldAsString String
    | MsgGotResult (Result Http.Error (Maybe Iteam))
    | MsgSuccessfulDelete (Result Http.Error ())



--    | MsgKeyPressed String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, cmdSearchAll )


initModel : Model
initModel =
    { iteamTitle = ""
    , iteamThumbnail = ""
    , iteamPages = 0 --Maybe Int maybe.... mora 0 difoltno
    , iteamLink = ""
    , iteamPublisher = ""
    , iteamTopicId = 1 --Mora 1 difoltno
    , iteamId = 1
    , poruka = ""
    , results = []
    , resultIteam = Nothing
    , errorMessage = Nothing
    , loading = False
    }


view : Model -> Html.Html Msg
view model =
    viewLayout model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgInputTitleField newTextInput ->
            ( { model | iteamTitle = newTextInput }, Cmd.none )

        MsgInputThumbnailField newThumbnail ->
            ( { model | iteamThumbnail = newThumbnail }, Cmd.none )

        MsgInputPagesFieldAsString newPages ->
            ( { model | iteamPages = Maybe.withDefault 0 (String.toInt newPages) }, Cmd.none )

        --JOS JE BOLJE DA SE RAZLOZI NA SLUCAJEVE I POSEBNO OBRADE UMJESTO Maybe.withDefault
        MsgInputLinkField newLink ->
            ( { model | iteamLink = newLink }, Cmd.none )

        MsgInputPublisherField newPublisher ->
            ( { model | iteamPublisher = newPublisher }, Cmd.none )

        MsgInputTopicIdFieldAsString newTopicId ->
            ( { model | iteamTopicId = Maybe.withDefault 0 (String.toInt newTopicId) }, Cmd.none )

        
        MsgAddIteam ->
            updateAddIteam model

        MsgDeleteIteam ->
            updateDeleteIteam model

        MsgShowIteam ->
            updateShowIteam model

        MsgGetIteams ->
            updateSve model

        MsgGetIteamsEnviroment->
            updateEnviroment model

        MsgGetIteamsHealth->
            updateHealth model

        MsgGetIteamsSport->
            updateSport model
        
        MsgGetIteamsTechnology->
            updateTechnology model

        MsgGetIteamsWorld->
            updateWorld model

        MsgGetIteamsLargest ->
            updateLargest model

    
        MsgInputIdFieldAsString newId ->
            ( { model | iteamId = Maybe.withDefault 1 (String.toInt newId) }, Cmd.none )

        
        MsgSuccesfulPost result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

                --poruka = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResults result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | results = data, errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResult result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | resultIteam = data, results = [], errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgSuccessfulDelete result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

                --poruka = data, errorMessage = Nothing }, Cmd.none )
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )


updateAddIteam : Model -> ( Model, Cmd Msg )
updateAddIteam model =
    ( { model | loading = True }, postIteams model )


updateDeleteIteam : Model -> ( Model, Cmd Msg )
updateDeleteIteam model =
    ( { model | loading = True }, cmdDeleteIteam model )


updateShowIteam : Model -> ( Model, Cmd Msg )
updateShowIteam model =
    ( { model | loading = True }, cmdShowIteam model )


updateSve : Model -> ( Model, Cmd Msg )
updateSve model =
    ( { model | loading = True }, cmdSearchAll )


updateEnviroment : Model -> ( Model, Cmd Msg )
updateEnviroment model =
    ( { model | loading = True }, cmdSearchEnviroment )


updateSport : Model -> ( Model, Cmd Msg )
updateSport model =
    ( { model | loading = True }, cmdSearchSport )


updateTechnology : Model -> ( Model, Cmd Msg )
updateTechnology model =
    ( { model | loading = True }, cmdSearchTechnology)


updateHealth: Model -> ( Model, Cmd Msg )
updateHealth model =
    ( { model | loading = True }, cmdSearchHealth )


updateWorld : Model -> ( Model, Cmd Msg )
updateWorld model =
    ( { model | loading = True }, cmdSearchWorld )


updateLargest : Model -> ( Model, Cmd Msg )
updateLargest model =
    ( { model | loading = True }, cmdSearchLargest )



--subscriptions : model -> Sub Msg
--subscriptions _ =
--    Browser.Events.onKeyPress keyPressed


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none






viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Just (E.rgb255 0x00 0x33 0x66)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (E.column [ E.padding 2 ]
            [ viewSearchBar model
            , viewErrorMessage model
            , viewResults model
            , viewResult model
            ]
        )


viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.column []
        [ E.row [ E.spacing 10, E.paddingXY 10 30, EBG.color (E.rgb255 0xFF 0xFF 0xFF) ]
            [ EI.search []
                { onChange = MsgInputTitleField
                , text = model.iteamTitle
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x55 0x200 0x00) ] (E.text "Title:")
                }
            , EI.search []
                { onChange = MsgInputThumbnailField
                , text = model.iteamThumbnail
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x55 0x200 0x00) ] (E.text "Image:")
                }
            , EI.search []
                { onChange = MsgInputPagesFieldAsString
                , text = String.fromInt model.iteamPages --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x55 0x200 0x00) ] (E.text "View:")
                }
            , EI.search []
                { onChange = MsgInputLinkField
                , text = model.iteamLink
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x55 0x200 0x00) ] (E.text "Link:")
                }
            , EI.search []
                { onChange = MsgInputPublisherField
                , text = model.iteamPublisher
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x55 0x200 0x00) ] (E.text "Author:")
                }
            , EI.search []
                { onChange = MsgInputTopicIdFieldAsString
                , text = String.fromInt model.iteamTopicId --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x55 0x200 0x00) ] (E.text "Topic Id:")
                }
            , viewAddIteamButton
            ]
        , E.row [ E.spacing 10, E.centerX, E.paddingXY 470 30, EBG.color (E.rgb255 0xFF 0xFF 0xFF) ]
            [ EI.search []
                { onChange = MsgInputIdFieldAsString
                , text = String.fromInt model.iteamId --PROMENITI DA MOZE MAYBE INT
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x55 0x200 0x00) ] (E.text "News Id:")
                }
            , viewDeleteIteamButton
            , viewShowIteamButton
            ]
        , E.row [ E.spacing 2, E.paddingXY 0 5 ]
            [ viewGetIteamsButton
            , viewGetIteamsEnviromentButton
            , viewGetIteamsSportButton
            , viewGetIteamsHealthButton
            , viewGetIteamsTechnologyButton
            , viewGetIteamsWorldButton
            , viewGetIteamsLargestButton
            , if model.loading then
                E.html loadingImage

              else
                E.none
            ]
        ]


loadingImage : Html.Html msg
loadingImage =
    S.svg
        [ SA.width "64px"
        , SA.height "64px"
        , SA.viewBox "0 0 48 48"
        ]
        [ S.circle
            [ SA.cx "24"
            , SA.cy "24"
            , SA.stroke "#6699AA"
            , SA.strokeWidth "4"
            , SA.r "8"
            , SA.fill "none"
            ]
            [ S.animate
                [ SA.attributeName "opacity"
                , SA.values "0;.8;0"
                , SA.dur "2s"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        ]


viewErrorMessage : Model -> E.Element msg
viewErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            E.text errorMessage

        Nothing ->
            E.none


viewResults : Model -> E.Element msg
viewResults model =
    E.wrappedRow [ E.spacing 12, E.centerX ]
        (List.map viewIteam model.results)


viewResult : Model -> E.Element msg
viewResult model =
    let
        iteamPlaceholder =
            case model.resultIteam of
                Just iteam ->
                    viewIteam iteam

                Nothing ->
                    E.none
    in
    E.wrappedRow [ E.spacing 12, E.centerX ]
        [ iteamPlaceholder ]


viewIteam : Iteam -> E.Element msg
viewIteam iteam =
    let
        titleE =
            E.paragraph [ EF.semiBold, E.paddingXY 0 12 ] [ E.text iteam.title ]

        thumbnailE =
            case iteam.thumbnail of
                Just thumbnail ->
                    viewIteamCover thumbnail iteam.title

                Nothing ->
                    E.none

        pagesE =
            case iteam.pages of
                Just pages ->
                    E.paragraph [ EF.size 12 ]
                        [ E.text ("(" ++ String.fromInt pages ++ " views)") ]

                Nothing ->
                    E.none

        publisherE =
            case iteam.publisher of
                Just publisher ->
                    E.paragraph [ EF.size 16 ]
                        [ E.text publisher ]

                Nothing ->
                    E.none
    in
    E.newTabLink
        [ E.width (E.px 360)
        , E.height (E.px 360)
        , EBG.color (E.rgb255 0xE3 0xEA 0xED)
        , EB.rounded 0
        , E.padding 15
        , E.mouseOver
            [ EBG.color (E.rgb255 0x55 0x200 0x00)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x55 0x200 0x00)
            ]
        ]
        { url = iteam.link
        , label =
            E.row [ E.centerX ]
                [ thumbnailE
                , E.column [ E.padding 20 ]
                    [ titleE
                    , publisherE
                    , pagesE
                    ]
                ]
        }


viewIteamCover : String -> String -> E.Element msg
viewIteamCover thumbnail title =
    E.image []
        { src = thumbnail
        , description = title
        }


viewButtonGeneric : String -> Msg -> E.Element Msg
viewButtonGeneric naziv msg =
    EI.button
        [ EBG.color (E.rgb255 0x55 0x200 0x00)
        , EF.color (E.rgb255 0x00 0x00 0x00)
        , EB.rounded 0
        , E.padding 17
        , E.mouseOver
            [ EBG.color (E.rgb255 0x70 0x250 0x70)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        , E.focused
            [ EBG.color (E.rgb255 0x70 0x250 0x70)
            , EF.color (E.rgb255 0x00 0x00 0x00)
            ]
        ]
        { onPress = Just msg
        , label = E.text naziv
        }


viewAddIteamButton : E.Element Msg
viewAddIteamButton =
    viewButtonGeneric "Add News " MsgAddIteam


viewDeleteIteamButton : E.Element Msg
viewDeleteIteamButton =
    viewButtonGeneric "Delete News " MsgDeleteIteam


viewShowIteamButton : E.Element Msg
viewShowIteamButton =
    viewButtonGeneric "Get News " MsgShowIteam


viewGetIteamsButton : E.Element Msg
viewGetIteamsButton =
    viewButtonGeneric "          All News          " MsgGetIteams


viewGetIteamsEnviromentButton : E.Element Msg
viewGetIteamsEnviromentButton =
    viewButtonGeneric "         Enviroment         " MsgGetIteamsEnviroment


viewGetIteamsHealthButton : E.Element Msg
viewGetIteamsHealthButton =
    viewButtonGeneric "         Health         " MsgGetIteamsHealth


viewGetIteamsSportButton : E.Element Msg
viewGetIteamsSportButton =
    viewButtonGeneric "         Sport         " MsgGetIteamsSport


viewGetIteamsTechnologyButton : E.Element Msg
viewGetIteamsTechnologyButton =
    viewButtonGeneric "         Technology         " MsgGetIteamsTechnology


viewGetIteamsWorldButton : E.Element Msg
viewGetIteamsWorldButton =
    viewButtonGeneric "         World         " MsgGetIteamsWorld



viewGetIteamsLargestButton : E.Element Msg
viewGetIteamsLargestButton =
    viewButtonGeneric "          Most viwed         " MsgGetIteamsLargest






cmdAddIteam : Model -> Cmd Msg
cmdAddIteam model =
    Http.request
        { method = "POST"
        , headers = [ header "Access-Control-Allow-Origin" "http://localhost:5000", header "Access-Control-Allow-Credentials" "true", header "Content-Type" "application/json" ]
        , url = "http://localhost:5000/iteams"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        , timeout = Nothing
        , tracker = Nothing
        }


postIteams : Model -> Cmd Msg
postIteams model =
    Http.post
        { url = "http://localhost:5000/iteams"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }


cmdDeleteIteam : Model -> Cmd Msg
cmdDeleteIteam model =
    Http.post
        { url = "http://localhost:5000/iteams/" ++ String.fromInt model.iteamId
        , body = emptyBody
        , expect = Http.expectWhatever MsgSuccessfulDelete
        }


cmdShowIteam : Model -> Cmd Msg
cmdShowIteam model =
    Http.get
        { url = "http://localhost:5000/iteams/" ++ String.fromInt model.iteamId
        , expect = Http.expectJson MsgGotResult (JD.maybe decodeItem)
        }


cmdSearchAll : Cmd Msg
cmdSearchAll =
    Http.get
        { url = "http://localhost:5000/iteams/sve"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchEnviroment : Cmd Msg
cmdSearchEnviroment =
    Http.get
        { url = "http://localhost:5000/iteams/enviroment"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchSport : Cmd Msg
cmdSearchSport =
    Http.get
        { url = "http://localhost:5000/iteams/sport"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchTechnology : Cmd Msg
cmdSearchTechnology =
    Http.get
        { url = "http://localhost:5000/iteams/technology"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchHealth : Cmd Msg
cmdSearchHealth =
    Http.get
        { url = "http://localhost:5000/iteams/health"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchWorld : Cmd Msg
cmdSearchWorld =
    Http.get
        { url = "http://localhost:5000/iteams/world"
        , expect = Http.expectJson MsgGotResults decodeItems
        }



cmdSearchLargest : Cmd Msg
cmdSearchLargest =
    Http.get
        { url = "http://localhost:5000/iteams/join"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "title", JE.string model.iteamTitle )
        , ( "thumbnail", JE.string model.iteamThumbnail )
        , ( "pages", JE.int model.iteamPages )
        , ( "link", JE.string model.iteamLink )
        , ( "publisher", JE.string model.iteamPublisher )
        , ( "topicId", JE.int model.iteamTopicId )
        ]


decodeItems : JD.Decoder (List Iteam)
decodeItems =
    JD.list decodeItem


decodeItem : JD.Decoder Iteam
decodeItem =
    JD.map5 Iteam
        (JD.field "title" JD.string)
        (JD.maybe (JD.field "thumbnail" JD.string))
        (JD.field "link" JD.string)
        (JD.maybe (JD.field "pages" JD.int))
        (JD.maybe (JD.field "publisher" JD.string))
