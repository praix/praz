module Network.Tor.Daemon where

import Prelude as P

run :: IO ()
run = do 
  P.putStrLn "running the Tor daemon..."

  -- read the config

  -- setup the OR

  -- listen for connections

  -- publish relay descriptor

  -- schedule regular key rotation

  -- schedule regular relay descriptor publishing



{-

implement:
* connections
* circuits 
* multiplexing

## init STEPS

-- setups the key material (identity key and temporary onion key)

-- initiate long term key
-- initiate onion key (temporary)

-- listen on the advertised TCP or port for incoming conns

## COMPONENTS


OR

  listener net.Listener
  config   *Config

  // Hold Fingerprint to OnionConnection mappings
  authenticatedConnections map[Fingerprint]*OnionConnection
  authConnLock             sync.Mutex

  descriptor tordir.Descriptor

  identityKey, onionKey   openssl.PrivateKey
  ntorPrivate, ntorPublic [32]byte

  clientTlsCtx, serverTlsCtx *TorTLS
  tlsLock                    sync.Mutex


## Onion connection

type OnionConnection struct {
  parentOR         *ORCtx
  readQueue        chan Cell
  circuitReadQueue CircReadQueue
  writeQueue       chan []byte
  circuits         map[CircuitID]*Circuit
  relayCircuits    map[CircuitID]*RelayCircuit

  usedTLSCtx        *TorTLS
  negotiatedVersion LinkVersion

  isOutbound          bool
  weAuthenticated     bool
  theyAuthenticated   bool
  theirFingerprint    Fingerprint
  theirFingerprint256 []byte
}

## Circuit

type Circuit struct {
  id                CircuitID
  forward, backward DirectionalCircuitState
  forwardWindow     int
  backwardWindow    *Window
  nextHop           CircReadQueue
  nextHopID         CircuitID

  streams     map[StreamID]*Stream
  extendState *CircuitHandshakeState
}

type RelayCircuit struct {
  id, theirID CircuitID
  previousHop CircReadQueue
}


#### Fingerprint

taken from ctor

/** Given a private or public key <b>pk</b>, put a fingerprint of the
 * public key into <b>fp_out</b> (must have at least FINGERPRINT_LEN+1 bytes of
 * space).  Return 0 on success, -1 on failure.
 *
 * Fingerprints are computed as the SHA1 digest of the ASN.1 encoding
 * of the public key, converted to hexadecimal, in upper case, with a
 * space after every four digits.
 *
 * If <b>add_space</b> is false, omit the spaces.
-}