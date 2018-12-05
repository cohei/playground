{-# LANGUAGE TypeFamilies #-}
-- | <https://keens.github.io/blog/2016/11/22/rustnokanrenkatanotsukaidokoro/ Rustの関連型の使いどころ>
module AssociatedType where

import           Data.Int  (Int64)
import           Data.Word (Word64)

class ToUnsigned a where
  type Unsigned a
  toUnsigned :: a -> Unsigned a

instance ToUnsigned Int64 where
  type Unsigned Int64 = Word64
  toUnsigned = fromIntegral

data HTTPHandler
data HTTPRequest
data HTTPResponse

{-
• Couldn't match type ‘Response a’ with ‘Response a0’
  Expected type: Request a -> Response a -> IO ()
    Actual type: Request a0 -> Response a0 -> IO ()
  NB: ‘Response’ is a non-injective type family
  The type variable ‘a0’ is ambiguous
• In the ambiguity check for ‘handle’
  To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
  When checking the class method:
    handle :: forall a. Handler a => Request a -> Response a -> IO ()
  In the class declaration for ‘Handler’

class Handler a where
  type Request a
  type Response a
  handle :: Request a -> Response a -> IO ()

instance Handler HTTPHandler where
  type Request HTTPHandler = HTTPRequest
  type Response HTTPHandler = HTTPResponse
  handle = undefined
-}

{-
• Could not deduce (Handler req res a0)
  from the context: Handler req res a
    bound by the type signature for:
               handle :: forall req res a.
                         Handler req res a =>
                         req -> res -> IO ()
    at src/WIP/AssociatedType.hs:45:3-32
  The type variable ‘a0’ is ambiguous
• In the ambiguity check for ‘handle’
  To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
  When checking the class method:
    handle :: forall req res a.
              Handler req res a =>
              req -> res -> IO ()
  In the class declaration for ‘Handler’

class Handler req res a | a -> req, a -> res where
  handle :: req -> res -> IO ()

instance Handler HTTPRequest HTTPResponse HTTPHandler where
  handle = undefined
-}
