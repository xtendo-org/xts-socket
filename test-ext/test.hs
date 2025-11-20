{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (unless, void, when)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.Int
import Data.Maybe (isJust, listToMaybe)
import Data.Word
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Family.Inet6
import System.Socket.Protocol.TCP
import System.Socket.Protocol.UDP
import System.Socket.Type.Datagram
import System.Socket.Type.Stream
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit

main :: IO ()
main = do
  enabledVar <- lookupEnv externalEnvVarName
  if envEnabled enabledVar
    then runTest
    else do
      putStrLn $
        "External network tests are disabled. Set "
          <> externalEnvVarName
          <> "=1 to enable."
      exitSuccess
 where
  envEnabled =
    maybe False $ \raw -> map toLower raw `elem` ["1", "true", "yes", "on"]
  runTest = do
    ipv6EnvVar <- lookupEnv ipv6EnvVarName
    ipv6Part <-
      if envEnabled ipv6EnvVar
        then do
          ipv6Available <- detectIpv6Connectivity
          return
            [ if ipv6Available
                then ipv6Test
                else ignoreTestBecause "No IPv6 connectivity" ipv6Test
            ]
        else return []
    defaultMain $
      localOption (mkTimeout defaultTimeoutMicros) $
        testGroup "external" ([tcpGithubHead, udpGoogleDns] <> ipv6Part)

-- Constants

externalEnvVarName :: String
externalEnvVarName = "XTS_SOCKET_EXTERNAL"

ipv6EnvVarName :: String
ipv6EnvVarName = "XTS_SOCKET_EXTERNAL_IPV6"

exampleQuery :: ByteString
exampleQuery =
  build $
    mconcat
      [ "\x12\x34\x01\x00\x00\x01\x00\x00\x00\x00\x00\x00"
      , encodeLabels ["example", "com"]
      , "\x00\x01\x00\x01"
      ]
 where
  encodeLabels parts = mconcat (map encode parts) <> B.word8 0
   where
    encode segment =
      B.word8 (fromIntegral $ B.length segment) <> B.byteString segment

defaultTimeoutMicros :: Integer
defaultTimeoutMicros = 30 * 1_000_000

githubHeadRequest :: ByteString
githubHeadRequest =
  "HEAD / HTTP/1.1\r\n\
  \Host: github.com\r\n\
  \User-Agent: xts-socket-tests/1\r\n\
  \Connection: close\r\n\r\n"

dnsUdpPort :: Word16
dnsUdpPort = 53

ipv6Target :: SocketAddress Inet6
ipv6Target = SocketAddressInet6 addr (fromIntegral dnsUdpPort) 0 0
 where
  addr = inet6AddressFromTuple (0x2606, 0x4700, 0x4700, 0, 0, 0, 0, 0x1111)

-- Test cases

tcpGithubHead :: TestTree
tcpGithubHead = testCase tcName $ withNetworkRetries $ do
  addressInfo <-
    getAddressInfo
      (Just "github.com")
      (Just "80")
      mempty
      :: IO [AddressInfo Inet Stream TCP]
  addr <-
    maybe
      (assertFailure "getAddressInfo returned no addresses")
      (return . socketAddress)
      $ listToMaybe addressInfo
  bracket (socket :: IO (Socket Inet Stream TCP)) close $ \sock -> do
    connect sock addr
    void $ send sock githubHeadRequest mempty
    response <- receive sock 4096 mempty
    when (B.null response) $
      assertFailure "github.com closed the connection before replying"
    unless ("HTTP/" `B.isPrefixOf` response) $
      assertFailure
        ( "unexpected HTTP response prefix: "
            <> show (B.take 20 response)
        )
 where
  tcName = "github.com:80 responds to HTTP HEAD"

udpGoogleDns :: TestTree
udpGoogleDns = testCase
  "8.8.4.4 answers DNS queries in UDP"
  $ withNetworkRetries
  $ bracket (socket :: IO (Socket Inet Datagram UDP)) close
  $ \sock -> do
    void $ sendTo sock exampleQuery mempty target
    (response, _) <- receiveFrom sock 512 mempty
    validateDnsResponse response
 where
  target = SocketAddressInet addr port
  addr = inetAddressFromTuple (8, 8, 4, 4)
  port = fromIntegral dnsUdpPort

ipv6Test :: TestTree
ipv6Test = testCase
  "2606:4700:4700::1111 answers DNS queries"
  $ withNetworkRetries
  $ bracket (socket :: IO (Socket Inet6 Datagram UDP)) close
  $ \sock -> do
    void $ sendTo sock exampleQuery mempty ipv6Target
    (response, _) <- receiveFrom sock 512 mempty
    validateDnsResponse response

-- Utilities

detectIpv6Connectivity :: IO Bool
detectIpv6Connectivity =
  (probe >> return True) `catch` handler
 where
  probe = bracket acquire close $ \sock -> connect sock ipv6Target
  acquire = socket :: IO (Socket Inet6 Datagram UDP)
  handler e
    | Just sockErr <- fromException e
    , sockErr == eHostUnreachable || sockErr == eNetworkUnreachable =
        return False
    | otherwise = throwIO e

withNetworkRetries :: IO a -> IO a
withNetworkRetries action = go maxAttempts initialDelay
 where
  maxAttempts = 3 :: Int32
  initialDelay = 500_000
  go attemptsLeft delayMicros = do
    outcome <- try action
    case outcome of
      Right result -> return result
      Left err
        | shouldRetry err && attemptsLeft > 0 -> do
            print err
            putStrLn $ "Retrying " <> show attemptsLeft <> " more times"
            threadDelay delayMicros
            go (attemptsLeft - 1) (delayMicros * 2)
        | otherwise -> throwIO err

shouldRetry :: SomeException -> Bool
shouldRetry err =
  isSocketException || isIoException
 where
  isSocketException = isJust (fromException err :: Maybe SocketException)
  isIoException = isJust (fromException err :: Maybe IOError)

build :: Builder -> ByteString
build = LB.toStrict . B.toLazyByteString

validateDnsResponse :: ByteString -> Assertion
validateDnsResponse bs = do
  when (B.length bs < 12) $
    assertFailure "DNS response shorter than header"
  unless (B.take 2 bs == B.take 2 exampleQuery) $
    assertFailure "mismatching DNS transaction ID"
  let flags = B.index bs 2
  unless (flags .&. 0x80 /= 0) $
    assertFailure "response bit not set"
  let rcode = B.index bs 3 .&. 0x0F
  when (rcode /= 0) $
    assertFailure ("DNS server returned error code " <> show rcode)
  let qd = decode16 bs 4
  when (qd /= 1) $
    assertFailure ("unexpected question count: " <> show qd)
  let an = decode16 bs 6
  when (an == 0) $
    assertFailure "DNS answer contained no records"
 where
  decode16 :: ByteString -> Int -> Word16
  decode16 bytes offset = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
   where
    hi = B.index bytes offset
    lo = B.index bytes (offset + 1)
