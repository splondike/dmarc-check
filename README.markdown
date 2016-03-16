A program for verifying your dmarc aggregate reports and emailing you a summary of results.

The DMARC[1] specification describes how email receipients should treat messages which fail SPF or DKIM checks (anti email forgery measures). Part of the specification describes how aggregate failure/success reports should be formatted, and where they should be sent to. This program parses those aggregate reports looking for failures.

[1] https://dmarc.org/resources/specification/

## Building

You should just need Stackage installed (https://www.stackage.org/). After that you should just be able to switch to the directory and run `stack build`.

To run it, use `stack exec -- dmarc-check`. You will need to fill in the configuration file `dmarc-check.conf` (using `example-dmarc-check.conf` as a template). The program assumes you have a dedicated mailbox assigned to dmarc aggregate reports.

## Automated tests

The application has some automated tests. To run these use `stack test`.
