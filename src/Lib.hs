{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Bifunctor
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types  (GToJSON)
import           Data.Kind
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits
import qualified Data.ByteString.Lazy.Char8 as BS8

p :: ToJSON a => a -> IO ()
p = BS8.putStrLn . encode

data User = User
    { userName           :: String
    , userAge            :: Int
    , userFavoriteAnimal :: String
    }
    deriving stock (Show, Generic)

data Dog = Dog
    { dogName :: String
    , dogAge :: Int
    , dogFavoritePerson :: String
    , dogDarkestSecret :: String
    }
    deriving stock (Show, Generic)
    deriving ToJSON via Codec (Generically AsIs) Dog

bob :: User
bob = User "Bob" 32 "cats"

dog :: Dog
dog = Dog "Redneck" 4 "Bob" "i am a good boy"

-- | A newtype wrapper that allows us to propagate changes to the
-- underlying encoding.
newtype Codec (tag :: k) (a :: Type) = Codec { unCodec :: a }

-- | We need to have a wrapper here with the specified type variable
-- ordering. The newtype wrapper can't be used with the TypeApplications
-- style because (I think?) it unrolls to this:
--
-- > Codec :: forall k (tag :: k) (a :: Type). a -> Codec tag a
--
-- and the type application is annoying.
codec :: forall tag a. a -> Codec tag a
codec = Codec

-- | We want to have an "as-is" thing, that does not modify the underlying
-- ToJSON instance at all.
data Underlying'

instance ToJSON a => ToJSON (Codec Underlying' a) where
    toJSON (Codec a) = toJSON a


-- | We also want a way to say "Use the Generic instance, please"
data Generically'

instance
    (GToJSON Zero (Rep a), Generic a, Typeable a)
  =>
    ToJSON (Codec Generically' a)
  where
    toJSON (Codec a) = genericToJSON defaultOptions a

-- Note how this isn't going to require a ToJSON instance on 'User'. We can
-- just delegate to the generic instance directly.
--

-- | Now, we might want options here. The tick/prime/whatever in
-- Generically' is a hint that we'll be doing something a tiny bit fancier.
-- We want to compose modifications to the 'defaultOptions' value. So we
-- add a type variable to 'Generically' and we'll recurse on that.
--
-- How do we do type-level recursion and reify values? With type classes!
data Generically next

instance
    (GToJSON Zero (Rep a), Generic a, Typeable a
    , ModifyOptions next
    )
  =>
    ToJSON (Codec (Generically next) a)
  where
    toJSON (Codec a) = genericToJSON (modifyOptions @next defaultOptions) a

-- | This type class allows us to modify options. This isn't obviously
-- recursive, but we can make it more obvious by starting with the base
-- case.
class ModifyOptions next where
    modifyOptions :: Options -> Options

-- | The base case to the recursion in ModifyOptions. Don't change them at
-- all!
data AsIs

instance ModifyOptions AsIs where
    modifyOptions = id


-- | A common thing you want to do is drop a prefix. We'll leave the prefix
-- as a type variable, and provide a next variable to allow composition.
data Drop prefix next

-- | The common structure of dropping a prefix is going to be pretty
-- boring, but we're going to have a number of *kinds* of prefixes that
-- we'll want to drop. So we create a delegation class, 'HasDrop', that
-- contains the actual dropping logic.
instance
    ( ModifyOptions next
    , HasDrop symbol
    )
  =>
    ModifyOptions (Drop symbol next)
  where
    modifyOptions options = next
        { fieldLabelModifier =
            hasDrop @symbol . fieldLabelModifier next
        }
      where
        next = modifyOptions @next options

-- | 'ModifyOptions (Drop symbol next)' delegates to this thing.
class HasDrop symbol where
    hasDrop :: String -> String

-- | If it's a 'Symbol', then we drop the symbol.
instance (KnownSymbol symbol) => HasDrop  symbol where
    hasDrop field =
        maybe field id (List.stripPrefix prefix field)
      where
        prefix = symbolVal (Proxy @symbol)


-- | But you often want to drop the type name. That's common.
-- So let's implement another helper that drops the type name.
data TypeName a

instance (Typeable a) => HasDrop (TypeName a) where
    hasDrop field =
        case someSymbolVal typeName of
            SomeSymbol (prxy :: Proxy symbol) ->
                hasDrop @symbol field
      where
        typeName =
            lowerFirst $ show (typeRepTyCon (typeRep (Proxy @a)))
        lowerFirst [] = []
        lowerFirst (c : cs) = toLower c : cs

--
-- Oh! Oh no. That actually does not work. The string is "User" for the
-- type name, but the field is "userThing"

-- | Now we also want to do some snake casing.
data SnakeCasing next

instance (ModifyOptions next) => ModifyOptions (SnakeCasing next) where
    modifyOptions options =
        next
            { fieldLabelModifier =
                snakeCase . fieldLabelModifier next
            }
      where
        next = modifyOptions @next options

-- ok, that's enough for generic modification.
-- What about modifying the actual JSON object?
-- Codec could use 'Underlying'' to delegate to a preexisting underlying
-- ToJSON instance.
--
--
--
-- Maybe we want to map all the fields to match some other format.

data Underlying next

instance (HasModifier next, ToJSON a) => ToJSON (Codec (Underlying next) a) where
    toJSON (Codec a) = valueModifier @next (toJSON a)

class HasModifier tag where
    valueModifier :: Value -> Value

instance HasModifier AsIs where
    valueModifier = id

data MapFields tag next

instance
    ( HasFieldMapper tag
    , HasModifier next
    )
  =>
    HasModifier (MapFields tag next)
  where
    valueModifier = overObjectFields (fieldMapper @tag) . valueModifier @next
      where
        overObjectFields f (Object o) =
            Object
            . HashMap.fromList
            . map (first f)
            . HashMap.toList $ o
        overObjecFields _ x = x

class HasFieldMapper tag where
    fieldMapper :: Text -> Text

instance (HasFieldMapper next) => HasFieldMapper (SnakeCasing next) where
    fieldMapper = Text.pack . snakeCase . Text.unpack

instance HasFieldMapper AsIs where
    fieldMapper = id

data OmitField sym next

instance
    ( HasModifier next
    , KnownSymbol sym
    )
  =>
    HasModifier (OmitField sym next)
  where
    valueModifier v = case v of
        Object o ->
            Object $ HashMap.delete (Text.pack (symbolVal (Proxy @sym))) o
        x ->
            x
-- λ> toJSON $ codec @(Generically (SnakeCasing (Drop (TypeName Dog) AsIs))) dog
-- Object (fromList [("darkest_secret",String "i am a good boy"),("age",Number 4.0),("name",String
--  "Redneck"),("favorite_person",String "Bob")])
-- λ> toJSON $ codec @(Underlying (OmitField "darkest_secret" AsIs)) $ codec @(Generically (SnakeC
-- asing (Drop (TypeName Dog) AsIs))) dog
-- Object (fromList [("age",Number 4.0),("name",String "Redneck"),("favorite_person",String "Bob")
-- ])
