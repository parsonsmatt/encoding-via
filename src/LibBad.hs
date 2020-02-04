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

module LibBad where

{- }
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

-- $> :set -XTypeApplications -XDataKinds
--
-- $> import Data.Aeson

data User = User
    { userName           :: String
    , userAge            :: Int
    , userFavoriteAnimal :: String
    }
    deriving stock (Show, Generic)

bob :: User
bob = User "Bob" 32 "cats"

-- $> bob

newtype Codec (tag :: k) (a :: Type) = Codec a

codec :: forall tag a. a -> Codec tag a
codec = Codec

instance
    (HasCodec tag, GToJSON Zero (Rep a), Generic a, Typeable a)
  =>
    ToJSON (Codec tag a)
  where
    toJSON (Codec a) = genericToJSON (getOptions @tag (Proxy @a)) a

class HasCodec tag where
    getOptions :: Typeable x => Proxy x -> Options

data Generically

instance HasCodec Generically where
    getOptions _ = defaultOptions

-- $> p $ codec @Generically bob

data DropPrefix (prefix :: k)

data TypeName

instance HasCodec (DropPrefix TypeName) where
    getOptions prxy = defaultOptions
        { fieldLabelModifier =
            drop (length (show (typeRepTyCon (typeRep prxy))))
        }

-- $> p $ codec @(DropPrefix TypeName) bob

instance (KnownSymbol sym) => HasCodec (DropPrefix (sym :: Symbol)) where
    getOptions prxy = defaultOptions
        { fieldLabelModifier =
            drop (length (symbolVal (Proxy @sym)))
        }

-- $> p $ codec @(DropPrefix "User") bob

instance (KnownNat nat) => HasCodec (DropPrefix (nat :: Nat)) where
    getOptions _ = defaultOptions
        { fieldLabelModifier =
            drop (fromIntegral (natVal (Proxy @nat)))
        }

-- $> p $ codec @(DropPrefix 4) bob

data WithCasing casing

data SnakeCase
data CamelCase

instance (HasCasing casing) => HasCodec (WithCasing casing) where
    getOptions _ = defaultOptions
        { fieldLabelModifier =
            casingFunction @casing
        }

class HasCasing casing where
    casingFunction :: String -> String

instance HasCasing SnakeCase where
    casingFunction = snakeCase

-- $> p $ codec @(WithCasing SnakeCase) bob

instance HasCasing CamelCase where
    casingFunction = camelCase
-}
