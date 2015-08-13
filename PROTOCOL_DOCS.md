

## Crypto

2 types of keys:

* long-term key. usage: sign TLS certificates; sign OR router descriptor (keys, address ,bandwidth, exit policy)
* short-term onion key. usage: decrypt requests for circuit creation and negotiate ephemeral keys


## Protocol steps

Listen for TCP connections 

Accept a connection

wrap connection with TLS

HandleHandshake

Server

  negotiate version
    read 5 bytes (deduce variable length cell)
    read length bytes
    validate version (fail if wrong)
    respond with selected version

  send certificates

  send auth challenge

  send net info

  handleCerts (response to auth challenge)

  Run loop
    listen for cells. route them correctly.
    listen for circuit Read cells . what is this thing

  Handle commands
    CMD_CREATE_FAST
      register a new circuit locally and create all the keys and whatnot

    CMD_RELAY, CMD_RELAY_EARLY
      handling either
        circuit OR relaycircuit
    CMD_CREATE, CMD_CREATE2
      register a new circuit locally and create all the keys and whatnot
    CMD_DESTROY
      well i guess we all know what this is
    CMD_CREATED, CMD_CREATED2
      notification that a forward circuit was created. i still don't understand fully 
    CMD_PADDING, CMD_VPADDING
      can be ignored therefore ignore

    disallowed commands
    CMD_CERTS, CMD_NETINFO, CMD_AUTH_CHALLENGE, CMD_AUTHORIZE, CMD_AUTHENTICATE

    otherwise drop cell

Client
  mirror of the above



    CMD_RELAY subcommands

      case RELAY_DATA:
    
      case RELAY_END:
      
      case RELAY_SENDME:
  
      case RELAY_BEGIN_DIR, RELAY_BEGIN:
    
      case RELAY_EXTEND:

    CMD_RELAY paths

      circuits - forward circuits
      relayCircuits - backward circuits ? wtf

### Cells

Cells are fixed size 512 (or 514 wtf) or variable size with a length prefix.

There are 2 versions of cells currently deployed.

* v3
* v4

