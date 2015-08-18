A program for verifying your dmarc aggregate reports and emailing you a summary of results.

The DMARC[1] specification describes how email receipients should treat messages which fail SPF or DKIM checks (anti email forgery measures). Part of the specification describes how aggregate failure/success reports should be formatted, and where they should be sent to. This program parses those aggregate reports looking for failures.

[1] https://dmarc.org/resources/specification/

## Building

You should just need the Haskell Platform installed (https://www.haskell.org/platform/). After that you should just be able to switch to the directory and run `cabal build`.

To run it, use `cabal run`. You will need to fill in the configuration file `dmarc-check.conf` (using `example-dmarc-check.conf` as a template). The program assumes you have a dedicated mailbox assigned to dmarc aggregate reports.
