module Main exposing (Model, Msg(..), constructURL, getImages, getImagesDecoder, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, src, target, value)
import Html.Events exposing (..)
import Http
import Icons
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Key exposing (key)
import Svg exposing (path, svg)
import Svg.Attributes exposing (class, clip, clipRule, d, fill, fillRule, height, viewBox, width)
import Url.Builder as Url



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--TYPES


type alias Image =
    { link : String
    }


type alias Item =
    { href : String
    , links : List Link
    , data : List ImageData
    }


type alias Link =
    { href : String
    }


type alias ImageData =
    { title : String
    , description : String
    , keywords : List String
    }


type GridSetting
    = OneCol
    | TwoCol
    | FourCol


type alias ResultItem =
    { link : String
    , data : ImageData
    }



-- MODEL


type alias Model =
    { term : String
    , url : String
    , images : List Item
    , isLoading : Bool
    , gridSetting : GridSetting
    , selectedItem : ResultItem
    , viewModal : Bool
    }


noItemSelected : ResultItem
noItemSelected =
    { link = ""
    , data =
        { title = ""
        , description = ""
        , keywords = [ "" ]
        }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        term =
            "black hole"
    in
    ( Model term
        "waiting.gif"
        []
        False
        TwoCol
        noItemSelected
        False
    , getImages term
    )



-- UPDATE


type Msg
    = FetchImages
    | NewImages (Result Http.Error (List Item))
    | UpdateSearchTerm String
    | KeyDown Int
    | SetGrid GridSetting
    | ItemSelected ResultItem
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchImages ->
            ( model
            , getImages model.term
            )

        NewImages result ->
            case result of
                Ok images ->
                    ( { model
                        | images = images
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        UpdateSearchTerm term ->
            ( { model | term = term }
            , Cmd.none
            )

        KeyDown key ->
            if key == 13 then
                ( { model
                    | images = []
                    , isLoading = True
                  }
                , getImages model.term
                )

            else
                ( model, Cmd.none )

        SetGrid newSetting ->
            case newSetting of
                OneCol ->
                    ( { model
                        | gridSetting = newSetting
                      }
                    , Cmd.none
                    )

                TwoCol ->
                    ( { model
                        | gridSetting = newSetting
                      }
                    , Cmd.none
                    )

                FourCol ->
                    ( { model
                        | gridSetting = newSetting
                      }
                    , Cmd.none
                    )

        ItemSelected item ->
            ( { model
                | selectedItem = item
                , viewModal = True
              }
            , Cmd.none
            )

        CloseModal ->
            ( { model
                | selectedItem = noItemSelected
                , viewModal = False
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        -- page title
        [ p [ class "siteTitle", id "top" ]
            [ text "Explore "
            , a [ href "https://images.nasa.gov/" ] [ text "images-api.nasa.gov" ]
            , text " with keyword search"
            ]

        -- search input and button
        , div [ class "searchGroup" ]
            [ input
                [ class "searchInput"
                , value model.term
                , onInput UpdateSearchTerm
                , onKeyDown KeyDown
                , placeholder "Search for images"
                , autofocus
                    True
                ]
                []
            ]

        -- results
        , div
            [ class (resultsGroupClassList model.gridSetting) ]
            -- if isLoading is true show the spinner, else show the results
            (case model.isLoading of
                True ->
                    [ div [ class "loader" ]
                        [ div [ class "line" ] []
                        , div [ class "line" ] []
                        , div [ class "line" ] []
                        ]
                    ]

                False ->
                    List.map renderItem model.images
            )
        , renderGridControls
        , case model.viewModal of
            True ->
                imageDetailsModal model.selectedItem

            False ->
                div [] []
        ]


imageDetailsModal : ResultItem -> Html Msg
imageDetailsModal item =
    div [ class "modalContainer" ]
        [ div [ class "modalBlanket", onClick CloseModal ] []
        , div [ class "selectedImageModal" ]
            [ div [ class "modalImage" ]
                [ img [ src item.link ] []
                , div [ class "modalImageDetails" ]
                    [ div [ class "modalDetailsBlock" ]
                        [ p [ class "modalHeading" ] [ text item.data.title ]
                        , p [ class "modelDescription" ] [ text item.data.description ]
                        ]
                    ]
                , div [ class "modalHeaderBlock" ]
                    [ button [ class "closeButton", onClick CloseModal ] [ renderCloseIcon ]
                    ]
                ]
            ]
        ]


renderKeywords : String -> Html msg
renderKeywords word =
    text (word ++ " ")


renderGridControls : Html Msg
renderGridControls =
    div [ class "gridSettings" ]
        [ a [ href "#top" ]
            [ button [ class "gridSettingButton" ] [ renderSearchIcon ]
            ]
        , button
            [ class "gridSettingButton"
            , onClick (SetGrid OneCol)
            ]
            [ text "I" ]
        , button
            [ class "gridSettingButton"
            , onClick (SetGrid TwoCol)
            ]
            [ text "II" ]
        , button
            [ class "gridSettingButton"
            , onClick (SetGrid FourCol)
            ]
            [ text "IIII" ]
        , a
            [ target "_blank"
            , href "https://github.com/hrryndrsn/spaceSearch"
            ]
            [ button
                [ class "gridSettingButton githubButton" ]
                [ renderGithubIcon ]
            ]
        ]


renderCloseIcon : Html msg
renderCloseIcon =
    svg
        [ class "searchIcon", width "17", height "17", viewBox "0 0 17 17", fill "none" ]
        [ path
            [ fillRule "evenodd", clipRule "evenodd", d Icons.close ]
            []
        ]


renderSearchIcon : Html msg
renderSearchIcon =
    svg
        [ class "searchIcon", width "14", height "14", viewBox "0 0 14 14", fill "none" ]
        [ path
            [ fillRule "evenodd", clipRule "evenodd", d Icons.search ]
            []
        ]


renderGithubIcon : Html msg
renderGithubIcon =
    svg
        [ class "searchIcon", width "16", height "16", viewBox "0 0 24 24", fill "none" ]
        [ path
            [ fillRule "evenodd", clipRule "evenodd", d Icons.github ]
            []
        ]


resultsGroupClassList : GridSetting -> String
resultsGroupClassList gridSetting =
    case gridSetting of
        OneCol ->
            "resultsGroup oneCol"

        TwoCol ->
            "resultsGroup twoCol"

        FourCol ->
            "resultsGroup fourCol"


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


renderItem : Item -> Html Msg
renderItem item =
    --Unwrap records from lists
    case getFirstItem item.links of
        Just link ->
            case getFirstItem item.data of
                Just imageData ->
                    --we know we have both img src url and image meta data
                    div [ class "item", onClick (ItemSelected (ResultItem link.href imageData)) ]
                        [ div [ class "itemImage" ]
                            [ img [ src link.href ] []
                            ]
                        , div [ class "itemDetails" ]
                            [ p [ class "itemName" ] [ text imageData.title ]
                            ]
                        ]

                Nothing ->
                    text ""

        Nothing ->
            text ""


getFirstItem : List a -> Maybe a
getFirstItem list =
    List.head list



-- HTTP


getImages : String -> Cmd Msg
getImages term =
    Http.send NewImages (Http.get (constructURL term) getImagesDecoder)


constructURL : String -> String
constructURL term =
    Url.crossOrigin "https://images-api.nasa.gov"
        [ "search" ]
        [ Url.string "q" term
        , Url.string "media_type" "image"
        ]


getImagesDecoder : Decode.Decoder (List Item)
getImagesDecoder =
    Decode.at [ "collection", "items" ] (Decode.list itemDecoder)


imageDecoder : Decode.Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "href" Decode.string


itemDecoder : Decode.Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "href" Decode.string
        |> required "links" (Decode.list linkDecoder)
        |> required "data" (Decode.list imageDataDecoder)


linkDecoder : Decode.Decoder Link
linkDecoder =
    Decode.succeed Link
        |> required "href" Decode.string


imageDataDecoder : Decode.Decoder ImageData
imageDataDecoder =
    Decode.succeed ImageData
        |> required "title" Decode.string
        |> optional "description" Decode.string "Classified"
        |> optional "keywords" (Decode.list Decode.string) [ "no keywords" ]
