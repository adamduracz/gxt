Generator-Driven XSLT Tester
===================

Using this tool it is possible to run automatic tests which check that, given input XML that adheres to a specified input XML schema, the output of an XSLT stylesheet will adhere to a specified output XML schema.

This tool is still experimental! Notable omissions from the XML Schema standard:

* Hierarchical XML schema
* Attribute groups
* Unicode character blocks and categories in XML schema regular expressions

How to compile
-------------

`cabal install`

How to run
---------

`gxt [OPTIONS] XSLT IN-SCHEMA OUT-SCHEMA`

How to view help
---------

`gxt --help`
