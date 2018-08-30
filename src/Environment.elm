module Environment exposing (Environment, OperatingSystem(..), Platform(..))


type alias Environment =
    { operatingSystem : OperatingSystem
    , platform : Platform
    }


type OperatingSystem
    = MacOS
    | Windows


type Platform
    = Zendesk
    | Web
