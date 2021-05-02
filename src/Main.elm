module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onResize)
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, el, fill, height, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)
import Html.Events exposing (on)
import Html.Lazy exposing (lazy, lazy2)
import Http
import InfiniteScroll as IS
import Json.Decode as JD exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (optional, required)
import Task


type Msg
    = Posts (Result Http.Error Response)
    | FetchPosts
    | RandPosts
    | WindowResize Int Int
    | InfiniteScrollMsg IS.Msg
    | WindowSize Viewport



---- MODEL ----


type alias Model =
    { children : PostList
    , query : String
    , error : String
    , before : String
    , after : String
    , loading : Bool
    , width : Int
    , height : Int
    , device : Device
    , infScroll : IS.Model Msg
    }


type alias Device =
    { class : DeviceClass
    , orientation : Orientation
    }


type DeviceClass
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice { height, width } =
    let
        y =
            if width <= 500 then
                Phone

            else if width > 500 && width < 980 then
                Tablet

            else
                Desktop

        z =
            if width < height then
                Portrait

            else
                Landscape
    in
    { class = y, orientation = z }


type Orientation
    = Portrait
    | Landscape


type alias Response =
    { children : PostList
    , after : String
    , before : String
    , subreddit : String
    }


type alias PostList =
    List Post


type PostHint
    = Video
    | Image
    | Link
    | Unknown
    | Gif


type alias Post =
    { id : String
    , imageUrl : String
    , postUrl : String
    , title : String
    , ups : Int
    , postHint : PostHint
    , source : String
    , iframe : String
    , url : String
    }


init : ( Model, Cmd Msg )
init =
    let
        query =
            "javascript"

        model =
            { children = []
            , query = query
            , error = ""
            , before = ""
            , after = ""
            , loading = False
            , width = 0
            , height = 0
            , device = { class = Phone, orientation = Portrait }
            , infScroll = IS.init (loadMore query "")
            }
    in
    ( { model | infScroll = IS.startLoading model.infScroll }
    , Cmd.batch [ Task.perform WindowSize Browser.Dom.getViewport, fetchPosts query "" ]
    )


loadMore : String -> String -> IS.Direction -> Cmd Msg
loadMore query after dir =
    fetchPosts query after


buildUrl : String -> String -> String
buildUrl query after =
    host ++ "/reddit?query=" ++ String.trim query ++ "&after=" ++ String.trim after


fetchPosts : String -> String -> Cmd Msg
fetchPosts query after =
    let
        url =
            buildUrl query after
    in
    request url


host : String
host =
    "https://redditcr.herokuapp.com"


postHintDecoder : Decoder PostHint
postHintDecoder =
    string
        |> JD.andThen
            (\str ->
                case str of
                    "rich:video" ->
                        JD.succeed Gif

                    "link" ->
                        JD.succeed Link

                    "image" ->
                        JD.succeed Image

                    _ ->
                        JD.succeed Unknown
            )


postHintToString : PostHint -> String
postHintToString pH =
    case pH of
        Gif ->
            "gif"

        Link ->
            "link"

        Image ->
            "image"

        _ ->
            "unknown"


postDecoder : Decoder Post
postDecoder =
    JD.succeed Post
        |> required "id" string
        |> optional "preview" string ""
        |> required "permalink" string
        |> required "title" string
        |> required "ups" int
        |> optional "post_hint" postHintDecoder Unknown
        |> optional "preview" string ""
        |> optional "media_embed" string ""
        |> optional "url" string ""


postsDecoder : Decoder Response
postsDecoder =
    JD.succeed Response
        |> required "children" (JD.list postDecoder)
        |> optional "after" string ""
        |> optional "before" string ""
        |> optional "subreddit" string ""


request : String -> Cmd Msg
request url =
    Http.get
        { url = url
        , expect = Http.expectJson Posts postsDecoder
        }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            let
                newModel =
                    { model | loading = True }
            in
            ( newModel, Cmd.batch [ fetchPosts newModel.query "" ] )

        RandPosts ->
            ( { model | children = [], query = "Loading..." }, Cmd.batch [ fetchPosts "randnsfw" "" ] )

        Posts (Err err) ->
            let
                infScroll =
                    IS.stopLoading model.infScroll

                newModel =
                    { model | infScroll = infScroll }
            in
            case err of
                Http.BadStatus status ->
                    ( { newModel | loading = False, error = String.fromInt status }, Cmd.none )

                Http.BadBody message ->
                    ( { newModel | loading = False, error = message }, Cmd.none )

                _ ->
                    ( { newModel | loading = False, error = "Something went wrong" }, Cmd.none )

        Posts (Ok response) ->
            let
                infScroll =
                    IS.stopLoading model.infScroll

                newModel =
                    { model
                        | children = model.children ++ response.children
                        , query = response.subreddit
                        , before = response.before
                        , after = response.after
                        , error = ""
                        , loading = False
                        , infScroll = infScroll |> IS.loadMoreCmd (loadMore response.subreddit response.after) |> IS.offset 300
                    }
            in
            ( newModel, Cmd.none )

        WindowResize x y ->
            ( { model | width = x, height = y, device = classifyDevice { width = x, height = y } }, Cmd.none )

        WindowSize viewport ->
            let
                w =
                    round viewport.viewport.width

                h =
                    round viewport.viewport.height

                w_ =
                    if w > 830 then
                        830

                    else
                        w

                device =
                    classifyDevice
                        { height = h
                        , width = w_
                        , x = 0
                        , y = 0
                        }
            in
            ( { model | width = w, height = h, device = device, infScroll = model.infScroll |> IS.offset 300 }, Cmd.none )

        InfiniteScrollMsg msg_ ->
            let
                ( infScroll, cmd ) =
                    IS.update InfiniteScrollMsg msg_ model.infScroll
            in
            ( { model | infScroll = infScroll }, cmd )



---- VIEW ----


mobileWidth w =
    width (px w |> Element.maximum 1024 |> Element.minimum 300)


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb255 0 0 0) ]
        (Element.column
            [ centerX
            , Element.inFront
                (el
                    [ padding 15
                    , Background.color (Element.rgba 0 0 0 0.2)
                    , whiteColor
                    , Element.alignBottom
                    , onClick RandPosts
                    ]
                    (Element.text model.query)
                )
            ]
            [ Element.html (lazy (\a -> Element.layout [] <| el [] a) (postContainer model)) ]
        )


whiteColor =
    Font.color (rgb255 255 255 255)


postContainer : Model -> Element Msg
postContainer model =
    Element.column
        [ mobileWidth model.width
        , Element.spacing 5
        , height (px <| model.height - 1)
        , Element.scrollbarY
        , Element.clipX
        , Element.htmlAttribute <| IS.infiniteScroll InfiniteScrollMsg
        ]
        (List.map (\x -> renderPost model.width x) model.children)


postHintStyle txt =
    Element.inFront (el [ Font.size 10, Element.moveDown 5, padding 5, Element.moveLeft 5, spacing 10, alignRight, Background.color (Element.rgba 0 0 0 0.2), whiteColor ] txt)


renderPost : Int -> Post -> Element msg
renderPost w post =
    row [ centerX ]
        [ if post.postHint == Unknown then
            Element.newTabLink [ padding 15, Background.color (Element.rgba 2 0 45 0.1) ]
                { url = post.url
                , label = Element.paragraph [ whiteColor, Font.size 12 ] [ Element.text post.title ]
                }

          else
            Element.newTabLink
                [ postHintStyle <| Element.text (postHintToString post.postHint)
                ]
                { url = post.url
                , label =
                    Element.image [ mobileWidth w, whiteColor ] { src = post.source, description = post.title }
                }
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ onResize WindowResize
                    ]
        }
