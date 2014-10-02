{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Control.Eff.JSON
where

import Data.Data (Typeable)
import Data.ByteString.Lazy hiding (unzip, empty)
import Data.Text hiding (empty)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Control.Eff

----------
-- Effects
----------

-- | Effect type for json input effects.
data JSONIn n
    = MaybeValue Text                       (Maybe Value -> n)
    | forall a. ThrowJSONError JSONError    (a -> n)
    deriving (Typeable)

instance Functor JSONIn where
    fmap f (MaybeValue t n)     = MaybeValue t (f . n)
    fmap f (ThrowJSONError e n) = ThrowJSONError e (f . n)

-- | An error occuring during the processing of the effects 
data JSONError
    = MissingProperty Text
    | CantDecodeProperty Text String -- ^ name of property and aeson error message
    | CantDecodeObject ByteString 
    deriving (Show)

maybeValue :: Member JSONIn r => Text -> Eff r (Maybe Value)
maybeValue t = send $ \ next -> inj (MaybeValue t next)

throwJSONError :: Member JSONIn r => JSONError -> Eff r a
throwJSONError err = send $ \ next -> inj (ThrowJSONError err next)


-- | Effect type for json output effects
data JSONOut n
    = forall a. ToJSON a => WriteProp Text a (a -> n)
    deriving (Typeable)

instance Functor JSONOut where
    fmap f (WriteProp t v n) =  WriteProp t v (f . n)

writeProp :: (Member JSONOut r, ToJSON a) => Text -> a -> Eff r a
writeProp p v = send $ \ next -> inj (WriteProp p v next)


------------
-- Operators
------------

-- | Write an ordinary value to the JSON-output
(<:) :: (ToJSON a, Member JSONOut r)
     => Text -> a -> Eff r a
(<:) = writeProp

infixl 0 <:

-- | Write effectful value to the JSON-output
(<$) :: (ToJSON a, Member JSONOut r)
      => Text -> Eff r a -> Eff r a
p <$ e = e >>= writeProp p

infixl 0 <$

-- | Write the outputs of some monads to a list in the output
(<$:) :: (ToJSON a, Member JSONOut r)
      => Text -> [Eff r a] -> Eff r [a] 
p <$: ms = do
    xs <- sequence . fmap extract $ ms
    let (json, res) = unzip xs
    p <: toJSON json
    return res

infixl 0 <$:

-- | Write the json output of the monad to the property
(<$.) :: Member JSONOut r 
      => Text -> Eff r a -> Eff r a
p <$. m = do
    (val, res) <- extract m
    p <: val
    return res

infixl 0 <$.

-- | Get the property if it is there.
maybeProp :: (FromJSON a, Member JSONIn r) 
          => Text -> Eff r (Maybe a)
maybeProp p = do
    val <- maybeValue p
    case val of
        Nothing -> return Nothing
        Just v -> case parseEither parseJSON v of
            Left err -> throwJSONError $ CantDecodeProperty p err
            Right res -> return . Just $ res

-- | Get the property or throw error if that is not possible
prop :: (FromJSON a, Member JSONIn r) 
     => Text -> Eff r a
prop p = do
    val <- maybeProp p
    case val of
        Nothing -> throwJSONError $ MissingProperty p
        Just val -> return val

-- | Use the value of a property to get a new monad. 
($>) :: (FromJSON a, Member JSONIn r) 
     => Text -> (a -> Eff r b) -> Eff r b
p $> m = prop p >>= m

infixl 0 $>

-- | Use the value of a property to get a new monad if it is there.
(?>) :: (FromJSON a, Member JSONIn r ) 
     => Text -> (a -> Eff r b) -> Eff r (Maybe b)
p ?> m = do
    val <- maybeProp p 
    case val of
        Nothing -> return Nothing
        Just v -> do
            res <- m v
            return $ Just res

infixl 0 ?>

-- | Use the value of a property as input for the monad.
(.$>) :: Member JSONIn r
      => Text -> Eff r a -> Eff r (Either JSONError a)
p .$> m = do
    obj <- prop p
    useObject obj m 

infixl 0 .$>

-- | Use the value of a property as input for the monad if it is there.
(.?>) :: Member JSONIn r
      => Text -> Eff r a -> Eff r (Maybe (Either JSONError a))
p .?> m = do
    obj' <- maybeProp p
    case obj' of
        Nothing -> return Nothing
        Just obj -> do
            res <- useObject obj m
            return . Just $ res

infixl 0 .?>

-- | Helper for composition, has higher fixity than read
--   and write operators.
(.$) = ($)
infixr 1 .$

-----------
-- Handlers
-----------

extract :: Member JSONOut r
        => Eff r a -> Eff r (Value, a) 
extract eff = go [] (admin eff)
    where
    go j (Val a) = return (object j, a)
    go j (E req) = interpose req (go j) $
        \ (WriteProp p v next) -> go ((p, toJSON v):j) (next v)

useObject :: Member JSONIn r
          => Object -> Eff r a -> Eff r (Either JSONError a)
useObject obj eff = go obj (admin eff)
    where
    go obj (Val a) = return . Right $ a
    go obj (E req) = interpose req (go obj) $
        \ req -> case req of
           MaybeValue t next        -> go obj (next $ HM.lookup t obj)
           ThrowJSONError err _     -> return $ Left err

runJSONIn :: Object -> Eff (JSONIn :> r) a -> Eff r (Either JSONError a) 
runJSONIn obj eff = go obj (admin eff)
    where
    go obj (Val a) = return . Right $ a
    go obj (E req) = handleRelay req (go obj) $
        \ req -> case req of
           MaybeValue t next        -> go obj (next $ HM.lookup t obj)
           ThrowJSONError err _     -> return $ Left err

runJSONOut :: Eff (JSONOut :> r) a -> Eff r (Value, a)
runJSONOut eff = go [] (admin eff)
    where
    go j (Val a) = return (object j, a)
    go j (E req) = handleRelay req (go j) $
        \ (WriteProp p v next) -> go ((p, toJSON v):j) (next v)
