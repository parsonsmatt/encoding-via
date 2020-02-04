{-# LANGUAGE TypeOperators, AllowAmbiguousTypes  #-}
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

-- $> :set -XTypeApplications -XTypeOperators -XDataKinds

p :: ToJSON a => a -> IO ()
p = BS8.putStrLn . encode

data User = User
    { userName           :: String
    , userAge            :: Int
    , userFavoriteAnimal :: String
    }
    deriving stock (Show, Generic)

bob :: User
bob = User "Bob" 32 "cats"

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

-- | Sometimes you just want to use the underlying representation.
type AsIs = ()

instance ToJSON a => ToJSON (Codec AsIs a) where
    toJSON (Codec a) = toJSON a

-- | Sometimes you want to use a generic representation.
data Generically

instance
    (GToJSON Zero (Rep a), Generic a, Typeable a)
  =>
    ToJSON (Codec Generically a)
  where
    toJSON (Codec a) = genericToJSON defaultOptions a

-- | Sometimes you want to modify a representation.
data a & b

infixr 6 &

instance
    (GToJSON Zero (Rep a), Generic a, Typeable a, ModifyOptions b)
  =>
    ToJSON (Codec (Generically & b) a)
  where
    toJSON (Codec a) = genericToJSON (modifyOptions @b defaultOptions) a

class ModifyOptions tag where
    modifyOptions :: Options -> Options

instance (ModifyOptions a, ModifyOptions b) => ModifyOptions (a & b) where
    modifyOptions = modifyOptions @a . modifyOptions @b

data Drop a

precomposeFields :: (String -> String) -> Options -> Options
precomposeFields f options = options
    { fieldLabelModifier =
        f . fieldLabelModifier options
    }

instance (KnownSymbol a) => ModifyOptions (Drop a) where
    modifyOptions =
        precomposeFields (drop (length (symbolVal (Proxy @a))))

data SnakeCase

instance ModifyOptions SnakeCase where
    modifyOptions = precomposeFields snakeCase

-- $> p $ codec @(Generically & SnakeCase & Drop "user") bob

data Dog = Dog
    { dogName :: String
    , dogAge :: Int
    , dogFavoritePerson :: String
    , dogDarkestSecret :: String
    }
    deriving stock (Show, Generic)
    deriving
        ToJSON
      via
        Codec
            (Censor (OmitField "darkest_secret")
                & Generically
                & SnakeCase
                & Drop "dog")
            Dog

dog :: Dog
dog = Dog "Redneck" 4 "Bob" "i am a good boy"

-- $> p dog
-- {"age":4,"name":"Redneck","favorite_person":"Bob"}

data Censor tag

instance (ToJSON (Codec rest a), HasCensor tag) => ToJSON (Codec (Censor tag & rest) a) where
    toJSON (Codec a) = censor @tag (toJSON (codec @rest a))

class HasCensor tag where
    censor :: Value -> Value

instance (HasCensor a, HasCensor b) => HasCensor (a & b) where
    censor = censor @a . censor @b

data OmitField symbol

instance (KnownSymbol symbol) => HasCensor (OmitField symbol) where
    censor val = case val of
        Object o ->
            Object
            . HashMap.delete (Text.pack (symbolVal (Proxy @symbol)))
            $ o
        _ ->
            val
