module CSVReport where

import Data.String (fromString)
import Data.List (nub, intercalate)
import Text.Printf (printf)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Bits as B
import qualified Data.ByteString as BS

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.IP as IP
import qualified Network.DNS as DNS
import qualified System.Time as T
import qualified Data.Csv as CSV

import qualified Data.DMARCAggregateReport as DM

buildReports :: [DM.Report] -> IO [CSVReport]
buildReports reports = do
   ipData <- fmap concat $ mapM resolveChunk ipWorkChunks
   return $ concat $ map (buildCsvReport ipData) reports
   where
      ipWorkChunks = chunk 4 distinctIps
      distinctIps = nub . map DM.sourceIp . concat $ map DM.records reports

buildCsvReport :: [(IP.IP, Maybe (String, Bool))] -> DM.Report -> [CSVReport]
buildCsvReport ipMap report = map (CSVReport . build) $ DM.records report
   where
      build record = [
         ("header_from", Just $ DM.headerFrom record),
         ("org_name", Just $ DM.orgName report),
         ("report_id", Just $ DM.reportId report),
         ("report_begin_date", Just $ toTimestamp $ DM.beginDate report),
         ("report_end_date", Just $ toTimestamp $ DM.endDate report),
         ("source_ip", Just $ show $ DM.sourceIp record),
         ("source_hostname", hostname $ DM.sourceIp record),
         ("source_lookup_sane", hostnameSane $ DM.sourceIp record),
         ("count", Just $ show $ DM.messageCount record),
         ("policy_dkim", Just $ show $ DM.policyDkim record),
         ("policy_spf", Just $ show $ DM.policySpf record)]
      toTimestamp (T.TOD timestamp _) = show timestamp
      hostname ip = lookup ip ipMap >>= id >>= return . fst 
      hostnameSane ip = lookup ip ipMap >>= id >>= return . boolToStr . snd 
      boolToStr True = "true"
      boolToStr False = "false"

-- | Resolve the list of IPs concurrently to a map of
-- IP -> (hostname, 'sane DNS')
--
-- 'sane DNS' means the PTR record for the IP matches what the A
-- or AAAA record it indicates says
resolveChunk :: [IP.IP] -> IO [(IP.IP, Maybe (String, Bool))]
resolveChunk chunk = mapM forkRunner chunk >>= mapM takeMVar
   where
      forkRunner ip = do
         rtn <- newEmptyMVar
         forkIO $ do
            rs <- DNS.makeResolvSeed DNS.defaultResolvConf
            -- TODO: Handle resolver errors
            result <- DNS.withResolver rs (handleLookup ip)
            putMVar rtn (ip, result)
         return rtn

handleLookup :: IP.IP -> DNS.Resolver -> IO (Maybe (String, Bool))
handleLookup ip resolver = do
   eitherDomains <- lookupReverseDNS resolver ip
   case eitherDomains of
        Right (domain:[]) -> do
           ips <- fetchIPRecords ip resolver domain
           return $ (eitherToMaybe $ decodeUtf8' domain) >>= \domainT ->
              return (stripTrailingPeriod $ unpack domainT, any (==ip) ips)
        _ -> return Nothing -- Errors and multiple PTR results
   where
      eitherToMaybe (Right val) = Just val
      eitherToMaybe (Left _) = Nothing
      stripTrailingPeriod "" = ""
      stripTrailingPeriod str = reverse $ dropWhile (=='.') $ reverse str

lookupReverseDNS :: DNS.Resolver -> IP.IP -> IO (Either DNS.DNSError [DNS.Domain])
lookupReverseDNS resolver ipUnion = DNS.lookupPTR resolver address
   where 
      address = case ipUnion of
                     (IP.IPv4 ip) -> ipv4Addr ip
                     (IP.IPv6 ip) -> ipv6Addr ip

      ipv4Addr ip = fromString $ (ipv4chunks ip) ++ ".in-addr.arpa"
      ipv4chunks ip = intercalate "." $ reverse $ map show $ IP.fromIPv4 ip

      ipv6Addr ip = fromString $ (ipv6chunks ip) ++ ".ip6.arpa"
      ipv6chunks ip = intercalate "." $ reverse $ toHexChars $ IP.fromIPv6b ip 
      toHexChars octets = map (:[]) $ concat $ map (printf "%02X") octets

fetchIPRecords :: IP.IP -> DNS.Resolver -> BS.ByteString -> IO [IP.IP]
fetchIPRecords ip resolver domain =
   case ip of
        (IP.IPv4 _) -> DNS.lookupA resolver domain >>=
                       return . eitherToList . fmap (map IP.IPv4)
        (IP.IPv6 _) -> DNS.lookupAAAA resolver domain >>=
                       return . eitherToList . fmap (map IP.IPv6)
   where
      eitherToList :: Either a [b] -> [b]
      eitherToList (Right v) = v
      eitherToList _ = []

chunk :: Int -> [a] -> [[a]]
chunk count xs = chunk' [] xs
   where
      chunk' acc [] = acc
      chunk' acc xs = let (curr, rest) = splitAt count xs
                      in chunk' (curr:acc) rest

newtype CSVReport = CSVReport [(String, Maybe String)]
   deriving (Show, Eq)

instance CSV.ToNamedRecord CSVReport where
   toNamedRecord (CSVReport pairs) = CSV.namedRecord $ map buildPair pairs
      where
         buildPair (name, Nothing) = (fromString name, BS.empty)
         buildPair (name, Just val) = (fromString name, fromString val)

instance CSV.DefaultOrdered CSVReport where
   headerOrder (CSVReport pairs) = CSV.header $ map (fromString . fst) pairs
