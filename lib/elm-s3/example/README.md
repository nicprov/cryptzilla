[S3Example.elm](S3Example.elm) is a simple example of using the S3 library in the [billstclair/elm-s3](http://package.elm-lang.org/packages/billstclair/elm-s3/latest) package.

In order to use it, you'll need to get a secret key and access key. You can get them on the [API Tokens](https://cloud.digitalocean.com/settings/api/tokens) page for DigitalOcean Spaces. There are multiple possibilities for Amazon AWS, but you'll probably want to create keys from the [Identity and Access Management (IAM)](https://console.aws.amazon.com/iam/) console. See the "Accounts File" section below for details.

You can run the example in Elm reactor:

    cd .../elm-s3/example
    elm reactor
    
Then aim your web browser at http://localhost:8000, to see this file, and click on [S3Example.elm](S3Example.elm) to run the code.

# Accounts File

[accounts.json.template](accounts.json.template) is a template for a file which describes your S3 accounts. Copy it to `accounts.json`, and edit to match your S3 account(s). It is a list of JSON objects, each of which has the following properties:

`"name"` is a string specifying the name to appear in the "Account" selection.

`"region"` is the region for the buckets. If omitted, it will use the global region.

`"is-digital-ocean"` is a boolean. If true, the account is on Digital Ocean Spaces. If false or omitted, the account is on Amazon S3.

`"access-key"` is a 20-character string specifying the account access key. This goes over the wire.

`"secret-key"` is a 40-character string specifying the account secret key. This is used for signing.

`"buckets"` is a list of strings, used to populate the "Buckets" selection.

For example:

    [{"name": "Digital Ocean",
      "region": "nyc3",
      "is-digital-ocean": true,
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "buckets": ["bucket1","bucket2"]
     },
     {"name": "Amazon S3",
      "region": "us-east-1",
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "buckets": ["bucket3","bucket4","bucket5"]
     }
    ]

