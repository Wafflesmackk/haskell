-- Daka Bence Neptun: I5RQFI
module Homework9 where
    --1 feladat
    newtype IPAddress = IP String deriving (Eq,Show,Ord)
    
    computer :: IPAddress
    computer = IP "192.168.0.15"
    
    --2 feladat
    newtype Bandwith = B Int deriving (Eq,Ord)
    defaultLink :: Bandwith
    defaultLink = B 10
    
    --3 feladat
    instance Show Bandwith where
        show (B k) = show k ++ " Mbps" 

    --4 feladat
    newtype MACAddress = MAC String deriving (Eq,Show,Ord)

    computerMac :: MACAddress
    computerMac = MAC "aa:bb:c8:88:dd:aa"

    --5 feladat
    data Address = AIP IPAddress | AMAC MACAddress deriving (Show, Eq, Ord)

    computerMacAddress :: Address
    computerMacAddress = AMAC (MAC "aa:bb:c8:88:dd:aa")

    computerIpAddress :: Address
    computerIpAddress = AIP (IP "192.168.0.15")

    --6 feladat
    data Reservation = R IPAddress MACAddress Int deriving(Show, Eq, Ord)
    
    reservation :: Reservation
    reservation = R(IP "192.168.0.15") (MAC "aa:bb:c8:88:dd:aa") 100

    --7 feladat
    dhcp :: MACAddress -> Maybe Reservation
    dhcp (MAC a)
        | length a == 17 =  (Just (R (IP "192.168.0.30") (MAC a) 100))
        | otherwise = Nothing