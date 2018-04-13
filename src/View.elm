module View exposing (..)

import Json.Encode
import Accessibility.Style exposing (invisible)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Models exposing (Model, Photo, Params)
import Msgs exposing (..)
import RemoteData exposing (WebData)
import Svg.Attributes exposing (d)
import Utils exposing (capitalize)
import Dict
import Array exposing (Array)
import String.Interpolate exposing (interpolate)


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
                    [ span
                        []
                        [ i [ class "fas fa-user fa-fw pr1" ] [], text "Owner: " ]
                    ]
                , dd [ class "dib ml1" ]
                    [ text owner ]
                ]

        Nothing ->
            text ""


photoTitle : Photo -> Html Msg
photoTitle photo =
    case photo.title of
        Just title ->
            dl [ class "f6 lh-title mv2" ]
                [ dt [ class "dib b" ]
                    [ span
                        []
                        [ i [ class "fas fa-file-alt fa-fw pr1" ] [], text "Title: " ]
                    ]
                , dd [ class "dib ml1" ]
                    [ text title ]
                ]

        Nothing ->
            text ""


licenseInfo : Photo -> Html Msg
licenseInfo photo =
    case photo.license of
        Just license ->
            dl [ class "f6 lh-title mv2" ]
                [ dt [ class "dib b" ]
                    [ span
                        []
                        [ i [ class "fas fa-gavel fa-fw pr1" ] [], text "License: " ]
                    ]
                , dd [ class "dib ml1" ]
                    [ a [ href license.url ] [ text license.name ] ]
                ]

        Nothing ->
            dl [ class "f6 lh-title mv2" ]
                [ dt [ class "dib b" ]
                    [ span
                        []
                        [ i [ class "fas fa-gavel fa-fw pr1" ] [], text "License: " ]
                    ]
                , dd [ class "dib ml1" ]
                    [ text "License unknown" ]
                ]


photoSource : Photo -> Html Msg
photoSource photo =
    dl [ class "f6 lh-title mv2" ]
        [ dt [ class "dib b" ]
            [ span
                []
                [ i [ class "fas fa-globe fa-fw pr1" ] [], text "Source: " ]
            ]
        , dd [ class "dib ml1" ]
            [ text (capitalize True photo.source) ]
        ]


textHtml : String -> Attribute Msg
textHtml s =
    Json.Encode.string s
        |> Html.Attributes.property "innerHTML"


attributionTemplate : String
attributionTemplate =
    let
        text =
            """
<h4>
  <span class="title">{0}</span>
  <i>by <span class="creator">{1}</span></i>
</h4>
<p class="info">
  Licensed under
  <a class="license" href="{2}">
    {3}
  </a>
</p>
<p>
  <a class="foreign_landing_url" href="{4}">
    Original source
  </a>
</p>
"""
    in
        String.trim text


attributionText : Photo -> String
attributionText photo =
    case ( photo.title, photo.license, photo.owner ) of
        ( Just title, Just license, Just owner ) ->
            interpolate attributionTemplate [ title, owner, license.url, license.name ]

        ( _, _, _ ) ->
            ""


photoAttribution : Photo -> Html Msg
photoAttribution photo =
    if requiresAttribution photo then
        div [ class "clip" ]
            [ pre [ id "attribution" ] [ text (attributionText photo) ] ]
    else
        text ""


requiresAttribution : Photo -> Bool
requiresAttribution photo =
    case photo.license of
        Just license ->
            case license.name of
                "Attribution-NonCommercial-ShareAlike License" ->
                    True

                "Attribution-NonCommercial License" ->
                    True

                "Attribution-NonCommercial-NoDerivs License" ->
                    True

                "Attribution License" ->
                    True

                "Attribution-ShareAlike License" ->
                    True

                "Attribution-NoDerivs License" ->
                    True

                _ ->
                    False

        Nothing ->
            False


copyButton : Photo -> Html Msg
copyButton photo =
    if requiresAttribution photo then
        a
            [ class "btn btnBlack mr2 copy-button pointer"
            , attribute "data-clipboard-target" "#attribution"
            ]
            [ span [] [ i [ class "fas fa-copy pr2" ] [], text "Copy" ] ]
    else
        text ""


visitButton : String -> Html Msg
visitButton url =
    a [ class "btn btnBlack mr2", href url, target "_blank" ]
        [ span [] [ i [ class "fas fa-globe pr2" ] [], text "Visit" ] ]


closeButton : Html Msg
closeButton =
    a [ class "btn btnBlack mr2 pointer", onClick StopViewing ]
        [ span [] [ i [ class "fas fa-times" ] [] ] ]


viewButton : String -> Html Msg
viewButton url =
    a [ class "btn btnBlack mr2", href url, target "_blank" ]
        [ span [] [ i [ class "fas fa-search pr2" ] [], text "View" ] ]


saveButton : String -> Html Msg
saveButton url =
    a [ class "btn btnBlack mr2" ]
        [ span [] [ i [ class "fas fa-star pr2" ] [], text "Save" ] ]


previousImage : Int -> Array Photo -> Html Msg
previousImage index photos =
    case Array.get (index - 1) photos of
        Just photo ->
            button
                [ class "btn btnBlack pointer"
                , onClick (Msgs.KeyMsg 37)
                ]
                [ span [] [ i [ class "fas fa-arrow-left fa-2x" ] [] ] ]

        Nothing ->
            div [] []


nextImage : Int -> Array Photo -> Html Msg
nextImage index photos =
    case Array.get (index + 1) photos of
        Just photo ->
            button
                [ class "btn btnBlack pointer"
                , onClick (Msgs.KeyMsg 39)
                ]
                [ span [] [ i [ class "fas fa-arrow-right fa-2x" ] [] ] ]

        Nothing ->
            div [] []


licenseTerms : Photo -> Html Msg
licenseTerms photo =
    -- [TODO] Implement these please
    case photo.license of
        Just license ->
            text "awesome"

        Nothing ->
            text "unknown"


imageGallery : Photo -> Model -> Html Msg
imageGallery photo model =
    let
        -- Reference data for previous/next buttons
        index =
            Maybe.withDefault 0 model.viewing

        photos =
            RemoteData.withDefault Array.empty model.photos
    in
        div
            [ id "imageGallery", class "w100 h100 flex flex-column bg-black" ]
            [ div [ class "w-100 pv3 flex justify-center ph2" ]
                [ visitButton photo.origin
                , viewButton photo.url
                , copyButton photo
                , saveButton photo.url
                , closeButton
                ]
            , div [ class "flex items-center justify-between h-100 pa3", style [ ( "flex-grow", "1" ) ] ]
                [ previousImage index photos
                , img [ class "imagePreview", src photo.url, alt "wew" ] []
                , nextImage index photos
                ]
            , div [ class "bg-moon-gray w-100 ph3" ]
                [ photoSource photo
                , photoTitle photo
                , photoOwner photo
                , licenseInfo photo
                , photoAttribution photo
                ]
            ]


optionsRibbon : Model -> Html Msg
optionsRibbon model =
    div
        [ class "items-baseline optionsRibbon mid-gray" ]
        [ span [ class "pr3 dib" ]
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
                    [ (dropDownButton "flickr" model)
                    , (dropDownButton "bing" model)
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
                    [ (licenseButton "Any" model)
                    , (licenseButton "Public" model)
                    , (licenseButton "Share" model)
                    , (licenseButton "Share Commercially" model)
                    , (licenseButton "Modify" model)
                    , (licenseButton "Modify Commercially" model)
                    ]
                ]
            ]
        ]


tick : Bool -> Html Msg
tick val =
    if val then
        i [ class "fas fa-check" ] []
    else
        text ""


dropDownButton : String -> Model -> Html Msg
dropDownButton source model =
    let
        btnStyle =
            if (Maybe.withDefault False (Dict.get source model.sources)) then
                style [ ( "font-weight", "bold" ), ( "color", "black" ) ]
            else
                style []

        ticked =
            tick (Maybe.withDefault False (Dict.get source model.sources))
    in
        li
            [ class "dropdown-button pointer"
            , onClick (ToggleSource source)
            , btnStyle
            ]
            [ span
                [ class "flex justify-between pv3 ph2 dropdown-button" ]
                [ span [] [ text (capitalize True source) ], ticked ]
            ]


matchLicenseDescription : String -> String
matchLicenseDescription desc =
    String.join "" (String.split " " (String.toLower desc))


licenseButton : String -> Model -> Html Msg
licenseButton description model =
    let
        licenseParam =
            matchLicenseDescription description

        btnStyle =
            if model.license == licenseParam then
                style [ ( "font-weight", "bold" ), ( "color", "black" ) ]
            else
                style []
    in
        li
            [ class "dropdown-button pointer"
            , onClick (UpdateLicense licenseParam)
            , btnStyle
            ]
            [ span
                [ class "flex justify-between pv3 ph2 dropdown-button" ]
                [ span []
                    [ text description ]
                , tick (model.license == licenseParam)
                ]
            ]


imageResults : Model -> Html Msg
imageResults model =
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
        , (imageGrid model)
        ]


resultsView : Model -> Html Msg
resultsView model =
    case ( model.viewing, model.photos ) of
        ( Just index, RemoteData.Success photos ) ->
            case Array.get index photos of
                Just photo ->
                    (imageGallery photo model)

                _ ->
                    imageResults model

        ( _, _ ) ->
            imageResults model


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
    case model.photos of
        RemoteData.Success photos ->
            if Array.isEmpty photos then
                let
                    text =
                        "No matches found for: " ++ "'" ++ model.previousSearch ++ "'"
                in
                    messageView text "search.svg"
            else
                let
                    photoThumbnail ( index, photo ) =
                        a [ class "imageContainer", onClick (ViewPhoto index) ]
                            [ img
                                [ src photo.thumbnail
                                , class "imageThumb w-100 h-100 dim"
                                ]
                                []
                            ]
                in
                    div
                        [ id "photoGrid"
                        , class "flex flex-wrap justify-center items-start"
                        ]
                        (Array.toList (Array.map photoThumbnail (Array.indexedMap (,) photos)))

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


messageView : String -> String -> Html msg
messageView message svg =
    div [ class "flex flex-column items-center justify-center h-100 bg-washed-yellow" ]
        [ img
            [ class "pb4"
            , src svg
            , Svg.Attributes.width "150"
            , Svg.Attributes.height "150"
            ]
            []
        , h1 [ class "fw6 f3 f2-ns lh-title mt0 mb3 amatic" ]
            [ text message ]
        ]


errorView : error -> Html msg
errorView error =
    messageView (toString error) "bug.svg"


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
