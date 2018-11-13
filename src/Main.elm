module Main exposing (Model, Msg(..), constructURL, getImages, getImagesDecoder, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, src, target, value)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
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
            [ fillRule "evenodd", clipRule "evenodd", d close ]
            []
        ]


renderSearchIcon : Html msg
renderSearchIcon =
    svg
        [ class "searchIcon", width "14", height "14", viewBox "0 0 14 14", fill "none" ]
        [ path
            [ fillRule "evenodd", clipRule "evenodd", d search ]
            []
        ]


renderGithubIcon : Html msg
renderGithubIcon =
    svg
        [ class "searchIcon", width "16", height "16", viewBox "0 0 24 24", fill "none" ]
        [ path
            [ fillRule "evenodd", clipRule "evenodd", d github ]
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



-- ICON SVG PATHS


search : String
search =
    "M8.992 10.0384C6.85392 11.6822 3.77655 11.5249 1.81822 9.56656C-0.31113 7.43722 -0.31113 3.98487 1.81822 1.85552C3.94756 -0.273822 7.39991 -0.273822 9.52926 1.85552C11.4309 3.75718 11.6343 6.71402 10.1393 8.84134L13.9292 12.6313L12.7571 13.8035L8.992 10.0384ZM9.08945 7.7915C8.92722 8.05737 8.7311 8.30839 8.50113 8.53836C8.3289 8.71059 8.14488 8.86382 7.95165 8.99805C6.39295 10.0809 4.23566 9.9277 2.84635 8.5384C1.28483 6.97688 1.28483 4.44515 2.84635 2.88363C4.40787 1.32211 6.93959 1.32211 8.50111 2.88363C9.83265 4.21517 10.0288 6.25216 9.08945 7.7915Z"


github : String
github =
    "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"


close : String
close =
    "M8.50001 9.91423L15.2929 16.7071L16.7071 15.2929L9.91423 8.50002L16.7071 1.70712L15.2929 0.292908L8.50001 7.0858L1.70712 0.292908L0.292908 1.70712L7.0858 8.50002L0.292908 15.2929L1.70712 16.7071L8.50001 9.91423Z"
