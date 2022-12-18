**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

**Status**

- 09-May-2021 - Published as version 9.0.0

The PATCH HttpMethod was added.

- 25-Aug-2020 - Published as version 8.1.0

The model for AWS service specs and JSON codecs for it were added.

- 17-Aug-2020 - Published as version 8.0.0

Error handling has been implemented. A separate error decoder can be specified
when building a request. There are ready implemented ones for the standard error
format, and for the case where no application level error is expected.

- 11-May-2020 - Published as version 6.0.0

The API has been redesigned. The intermediate `.Core.` module name has been
removed. The old `Encode` and `Decode` modules were dropped as not very useful
and poorly designed.

A new `KVEncode` module has been introduced to help with building headers and
query parameters. A `KVDecoder` module has been introduced to help decoding
response headers, it is simpler than `KVEncode` as less elaborate encoding
schemes are needed.

AWS URI encoding was taken from the old `Encode` module and put in its own `Uri`
module.

`ServiceConfig` and `Credentials` were made into type aliases instead of opaque
custom types. This mean the accessor functions could be dropped. I don't see
any reason to make these things opaque, there is no advantage in hiding
the implementation.

`Service` building was rationalized to remove some oddities. The `ServiceConfig`
is now a simple record and easy to understand. The defaulting is explained in
the docs. The defaulting happens behind the scenes when turning a config into a
`Service`.

An amount of unused code was deleted. The API is now leaner and cleaner.

# elm-aws-core

This package provides the functionality needed to make HTTP requests to AWS
services.

All AWS service calls must be signed correctly, in order to pass on the
authorized credentials of the caller to the service. AWS has multiple signing
schemes that different services use, specifically 'S3' and 'V4'.

The AWS service portfolio is large with variations in signing schemes, AWS
regions and service protocols across it. The aim of this package is to provide
functions to build signed HTTP requests correctly for all of the services
available on AWS. The specific service interface can then be implemented with
this package as a foundational element.

## Modules in this package

  - [AWS.Config](AWS-Config): Build a `ServiceConfig` describing the
  protocol, signing scheme, base URL and so on for a service.
  - [AWS.Service](AWS-Service): Turn a `ServiceConfig` into a `Service`
  definition, needed to make HTTP calls to that service.
  - [AWS.Http](AWS-Http): Build requests, sign and send them and decode the
  responses. Signing and sending a request requires both a `Service` and
  some `Credentials`.
  - [AWS.Credentials](AWS-Credentials): Create AWS `Credentials` used to sign
  requests.
  - [AWS.KVEncode](AWS-KVEncode): Utility for helping to encode Elm data into
  key-valued string pairs, for setting query parameters or header fields.
  - [AWS.KVDecode](AWS-KVDecode): Utility for decoding key-values string pairs
  into Elm data. Simpler than `KVEncode` as only needed for interpreting fairly
  simple response headers.
  - [AWS.Uri](AWS-Uri): Utility for URI encoding specific to how AWS does it.
