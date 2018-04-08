module View exposing (..)

import Json.Encode
import Accessibility.Style exposing (invisible)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit, on)
import List
import Models exposing (Model, Photo, Params)
import Msgs exposing (..)
import RemoteData exposing (WebData)
import Set as S
import Svg.Attributes exposing (d)


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        Models.HomeRoute query _ ->
            case query of
                Just query ->
                    resultsView model

                Nothing ->
                    homeView model

        Models.AboutRoute ->
            aboutView model

        Models.NotFoundRoute ->
            notFoundView


notFoundView : Html msg
notFoundView =
    div [] [ text "Not found" ]


resultsView : Model -> Html Msg
resultsView model =
    case model.error of
        Just _ ->
            div []
                [ div [] [ text "Something went wrong" ] ]

        Nothing ->
            resultsPage model (imageGrid model)


sourceIcon : String -> Html Msg
sourceIcon source =
    case String.toLower source of
        "flickr" ->
            i [ class "fab fa-flickr" ] []

        "bing" ->
            i [ class "fab fa-windows" ] []

        _ ->
            i [ class "fab fa-question-circle" ] []


photoDescription : Photo -> Html Msg
photoDescription photo =
    case photo.description of
        Just description ->
            let
                markup =
                    if String.isEmpty (String.trim description) then
                        "n/a"
                    else
                        description
            in
                dl [ class "f6 lh-title mv2" ]
                    [ dt [ class "dib b" ]
                        [ text "Description:" ]
                    , dd
                        [ class "dib ml1 photoDescription"
                        , property "innerHTML" <| Json.Encode.string markup
                        ]
                        []
                    ]

        Nothing ->
            text "-"


photoOwner : Photo -> Html Msg
photoOwner photo =
    case photo.owner of
        Just owner ->
            dl [ class "f6 lh-title mv2" ]
                [ dt [ class "dib b" ]
                    [ text "Owner:" ]
                , dd [ class "dib ml1" ]
                    [ text owner ]
                ]

        Nothing ->
            text ""


licenseInfo : Photo -> Html Msg
licenseInfo photo =
    case photo.license of
        Just license ->
            dl [ class "f6 lh-title mv2" ]
                [ dt [ class "dib b" ]
                    [ text "License:" ]
                , dd [ class "dib ml1" ]
                    [ a [ href license.url, class "white" ] [ text license.name ] ]
                ]

        Nothing ->
            text ""


textHtml : String -> Attribute Msg
textHtml s =
    Json.Encode.string s
        |> Html.Attributes.property "innerHTML"


copyButton : String -> Html Msg
copyButton url =
    a [ class "btn btnBlack mr2" ]
        [ span [] [ i [ class "fas fa-link pr2" ] [], text "Copy" ] ]


viewButton : String -> Html Msg
viewButton url =
    a [ class "btn btnBlack mr2", href url ]
        [ span [] [ i [ class "fas fa-search pr2" ] [], text "View" ] ]


imageModal : Photo -> Html Msg
imageModal photo =
    div
        [ id "imageModal" ]
        [ div [ class "modal-content" ]
            [ span
                [ class "pointer close f2 moon-gray absolute right-1 top-1"
                , onClick StopViewing
                ]
                [ text "×" ]
            , div [ class "flex ma4" ]
                [ img [ class "imagePreview", src photo.url ] []
                , div [ class "white flex flex-column items-start flex-start mb2 pl4" ]
                    [ span [ class "white f2 pb2" ]
                        [ text "Details" ]
                    , div
                        [ style [ ( "flex-grow", "1" ) ] ]
                        [ dl [ class "f6 lh-title mv2" ]
                            [ dt [ class "dib b" ]
                                [ text "Source:" ]
                            , dd [ class "dib ml1" ]
                                [ text photo.source ]
                            ]
                        , photoDescription photo
                        , photoOwner photo
                        , licenseInfo photo
                        ]
                    , div []
                        [ viewButton photo.url
                        , copyButton photo.url
                        ]
                    ]
                ]
            ]
        ]


getParam : Params -> String -> String -> Bool
getParam params key val =
    case (Dict.get key params) of
        Just values ->
            S.member val values

        Nothing ->
            False


optionsRibbon : Model -> Html Msg
optionsRibbon model =
    div
        [ class "items-baseline optionsRibbon mid-gray" ]
        [ span [ class "pr2 dib" ]
            [ i [ class "fas fa-filter fa-fw pr1" ] []
            , span [ class "b" ] [ text "Filters" ]
            ]
        , div [ class "dropdown pr3" ]
            [ span
                [ class "mid-gray b" ]
                [ span [ class "pr1" ]
                    [ text "Sources" ]
                , span [ class "fas fa-caret-down" ] []
                ]
            , div [ class "dropdown-content" ]
                [ ul
                    [ style
                        [ ( "list-style-type", "none" )
                        , ( "padding", "0" )
                        , ( "margin", "0" )
                        ]
                    , class "f6"
                    ]
                    [ (dropDownButton "Flickr" (ToggleSource "flickr") model)
                    , (dropDownButton "Bing" (ToggleSource "bing") model)
                    ]
                ]
            ]
        , div [ class "dropdown" ]
            [ span
                [ class "mid-gray b" ]
                [ span [ class "pr1" ]
                    [ text "Usage" ]
                , span [ class "fas fa-caret-down" ] []
                ]
            , div [ class "dropdown-content" ]
                [ ul
                    [ style
                        [ ( "list-style-type", "none" )
                        , ( "padding", "0" )
                        , ( "margin", "0" )
                        ]
                    , class "f6"
                    ]
                    [ (licenseButton "Any" UpdateLicense model)
                    , (licenseButton "Share" UpdateLicense model)
                    , (licenseButton "Share Commercially" UpdateLicense model)
                    , (licenseButton "Modify" UpdateLicense model)
                    , (licenseButton "Modify Commercially" UpdateLicense model)
                    ]
                ]
            ]
        ]


tick : Bool -> Html Msg
tick val =
    if val then
        span [ class "fas fa-check" ] []
    else
        text ""


dropDownButton : String -> Msg -> { a | sources : S.Set String } -> Html Msg
dropDownButton description msg model =
    let
        thing =
            String.toLower description

        btnStyle =
            if (S.member thing model.sources) then
                style [ ( "font-weight", "bold" ), ( "color", "black" ) ]
            else
                style []

        ticked =
            tick (S.member thing model.sources)
    in
        li
            [ class "dropdown-button pointer"
            , onClick msg
            , btnStyle
            ]
            [ span
                [ class "flex justify-between pv3 ph2 dropdown-button" ]
                [ span [] [ text description ], ticked ]
            ]


matchLicenseDescription : String -> String
matchLicenseDescription desc =
    case desc of
        "Any" ->
            "public"

        _ ->
            String.join "" (String.split " " (String.toLower desc))


licenseButton description msg model =
    let
        licenseParam =
            matchLicenseDescription description

        ticked =
            tick (model.license == licenseParam)

        btnStyle =
            if model.license == licenseParam then
                style [ ( "font-weight", "bold" ), ( "color", "black" ) ]
            else
                style []
    in
        li
            [ class "dropdown-button pointer"
            , onClick (msg licenseParam)
            , btnStyle
            ]
            [ span
                [ class "flex justify-between pv3 ph2 dropdown-button" ]
                [ span [] [ text description ], ticked ]
            ]


resultsPage : Model -> Html Msg -> Html Msg
resultsPage model html =
    let
        viewImage =
            case model.viewing of
                Just photo ->
                    (imageModal photo)

                Nothing ->
                    text ""
    in
        div [ class "h-100" ]
            [ div
                [ class "pv2 ph3 flex justify-between items-center bb"
                , id "resultsHeader"
                ]
                [ div [ class "w-100 flex items-center" ]
                    [ a [ href "/", class "mr3" ]
                        [ img
                            [ src "shutter.svg"
                            , Svg.Attributes.width "50"
                            , Svg.Attributes.height "50"
                            , class "dim"
                            ]
                            []
                        ]
                    , span [ class "mw7 w-100" ] [ (searchBar model) ]
                    ]
                ]
            , (optionsRibbon model)
            , viewImage
            , html
            ]


checkbox : String -> Msg -> Bool -> Html Msg
checkbox description msg default =
    span [ class "optionCheckBox" ]
        [ input
            [ type_ "checkbox"
            , style [ ( "vertical-align", "text-bottom" ) ]
            , onClick msg
            , checked default
            ]
            []
        , label [] [ text description ]
        ]


imageGrid : Model -> Html Msg
imageGrid model =
    let
        photoThumbnail ( index, photo ) =
            a [ class "imageContainer", onClick (ViewPhoto photo) ]
                [ img
                    [ src photo.url
                    , class "imageThumb w-100 h-100 dim"
                    ]
                    []
                ]
    in
        case model.photos of
            RemoteData.Success photos ->
                if List.isEmpty photos then
                    div [ class "flex flex-column items-center justify-center h-100 bg-washed-yellow" ]
                        [ img
                            [ class "pb4"
                            , src "search.svg"
                            , Svg.Attributes.width "150"
                            , Svg.Attributes.height "150"
                            ]
                            []
                        , h1 [ class "fw6 f3 f2-ns lh-title mt0 mb3 amatic" ]
                            [ text ("No matches found for: " ++ "'" ++ model.previousSearch ++ "'") ]
                        ]
                else
                    div
                        [ id "photoGrid"
                        , class "flex flex-wrap justify-center items-start"
                        ]
                        (List.map photoThumbnail (List.indexedMap (,) photos))

            RemoteData.Loading ->
                loadingView

            RemoteData.NotAsked ->
                text ""

            RemoteData.Failure error ->
                (errorView error)


loadingView : Html msg
loadingView =
    div [ class "flex flex-column items-center justify-center h-100" ]
        [ div
            [ class "pb4 mb2 dark-gray" ]
            [ span [ class "fas fa-spinner fa-spin fa-10x" ] [] ]
        , h1 [ class "fw6 f3 f2-ns lh-title mt0 mb3 amatic" ] [ text "Loading" ]
        ]


errorView : error -> Html msg
errorView error =
    div [ class "flex flex-column items-center justify-center h-100 bg-washed-yellow" ]
        [ img
            [ class "pb4"
            , src "bug.svg"
            , Svg.Attributes.width "150"
            , Svg.Attributes.height "150"
            ]
            []
        , h1 [ class "fw6 f3 f2-ns lh-title mt0 mb3 amatic" ]
            [ text "Something went wrong" ]
        ]


aboutView : Model -> Html msg
aboutView model =
    div [ class "aboutContainer" ]
        [ h1 [ class "tc f4" ]
            [ text "About" ]
        , p [ class "lh-copy measure center f6 black-70" ]
            [ text "Photosuggest is a project to help find permissively licensed images" ]
        , p [ class "lh-copy measure center f6 black-70" ]
            [ text "Icons made by "
            , a [ href "http://www.freepik.com", title "Freepik" ]
                [ text "Freepik" ]
            , text " from "
            , a [ href "https://www.flaticon.com/", title "Flaticon" ]
                [ text "www.flaticon.com" ]
            , text " is licensed by "
            , a
                [ href "http://creativecommons.org/licenses/by/3.0/"
                , target "_blank"
                , title "Creative Commons BY 3.0"
                ]
                [ text "CC 3.0 BY" ]
            ]
        ]


homeView : Model -> Html Msg
homeView model =
    let
        svgImage =
            if String.isEmpty model.query then
                "shutter.svg"
            else
                "coloured-shutter.svg"
    in
        div [ id "homeContainer" ]
            [ div [ id "searchWrapper", class "w-40-l w-70-m w-80-ns w-100" ]
                [ img
                    [ src svgImage
                    , Svg.Attributes.width "150"
                    , Svg.Attributes.height "150"
                    ]
                    []
                , h1 [ class "mv3", id "logoHeader", class "f1" ] [ text "Photosuggest" ]
                , (searchBar model)
                ]
            ]


searchBar : Model -> Html Msg
searchBar model =
    Html.form [ onSubmit SearchSubmit, class "w-100" ]
        [ fieldset [ class "bn p0 m0" ]
            [ span [ id "searchInputWrapper" ]
                [ label [ invisible, for "q" ] [ text "Enter search term" ]
                , (searchInput model)
                , (cancelBtn model)
                , searchSubmitBtn model.query
                ]
            ]
        ]


searchInput : Model -> Html Msg
searchInput model =
    input
        [ name "q"
        , id "searchInput"
        , size 1
        , type_ "text"
        , value model.query
        , onInput UpdateQuery
        , tabindex 1
        ]
        []


cancelBtn : Model -> Html Msg
cancelBtn model =
    if String.isEmpty model.query then
        text ""
    else
        button
            [ id "cancelBtn"
            , tabindex 3
            , onClick ClearInput
            , type_ "reset"
            ]
            [ i [ id "clearButton", class "fas fa-times" ] [] ]


searchSubmitBtn : String -> Html msg
searchSubmitBtn query =
    let
        buttonColours query =
            if String.isEmpty query then
                "bg-white hover-bg-green gray hover-white bg-animate "
            else
                "bg-green white hover-bg-dark-green bg-animate"
    in
        button
            [ id "searchSubmitBtn"
            , class (buttonColours query)
            , required True
            , tabindex 2
            ]
            [ i [ class "fas fa-search" ] [] ]
