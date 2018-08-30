module Session exposing (Session, fromNavKey, navKey)

import Browser.Navigation as Nav



-- TYPES


type Session
    = Anonymous Nav.Key



-- INFO


navKey : Session -> Nav.Key
navKey (Anonymous key) =
    key


fromNavKey : Nav.Key -> Session
fromNavKey key =
    Anonymous key
