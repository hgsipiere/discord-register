## Poke
### Verification bot for registers with full name and email

Poke is an interactive verification bot for registers with full name, email and photo ID requirements.

####Features:
- All verification forms sent to a verification channel
- Messages configurable with Dhall (except most admin messages in English)
- Verification forms can be reset by those with admin role using a command giving a reason
- Parser for full names, supporting unicode and requiring an underscore if no surname.
- Low memory usage (high cpu/memory usage for compile however)
- Written in Haskell using Polysemy with Calamity effects
- No database needed

To build run:
```stack build```

This build depends on `libtinfo-dev` for Ubuntu 20.04. 

The configuration is in ```config.dhall```, this needs to be copied and filled in from ```config.dhall.empty```.

Bot tokens can be found in the Discord Developer Portal after creating a bot.

ID numbers, or Snowflakes, can be found in the UI by right clicking on something after enabling developer mode in Appearance under the settings.
