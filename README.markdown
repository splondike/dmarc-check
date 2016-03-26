A program for converting DMARC aggregate reports kept in a dedicated IMAP mailbox to a CSV file.

The DMARC[1] specification describes how email receipients should treat messages which fail SPF or DKIM checks (anti email forgery measures). Part of the specification describes how aggregate failure/success reports should be formatted, and where they should be sent to. This program parses those aggregate reports and produces a CSV output.

[1] https://dmarc.org/resources/specification/

## Building

You should just need Stackage installed (https://www.stackage.org/). After that you should just be able to switch to the directory and run `stack build`. This will tell you where the (static) binary is if you want to copy it somewhere. You will need to make a configuration file using `example-dmarc-check.conf` as a template, and then use it like `./dmarc-check --config my-config.conf`. Additional options can be seen by running `./dmarc-check --help`.

To run the full program during development, use `stack exec -- dmarc-check --config config-file.conf`. You will need to run `stack build` to update the executable stack exec uses.

## Automated tests

The application has some automated tests. To run these use `stack test`.
