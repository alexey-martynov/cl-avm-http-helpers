"cl-avm-http-helpers"

This project contains different utilities to handle some tricky aspects
of the HTTP protocol.

HTTP Headers
------------

At this time library provides handling of the `Accept` header.

A typical usage looks like:

    (dispatch-mime-type header
      ("text/html"
        ; HTML generator
       )
      ("text/plain"
        ; Plain text generator
       )
      (otherwise
        ; Return `Not Acceptable` error
       ))

All MIME types from cases are got q-value from `Accept` header. The type
with the biggest q-value will be selected. Q-values to types assigned from
the most specific media ranges from teh `Accept` header.

If any two media has got the same q-value, first one will be selected. 
For example, if "text/html" and "text/plain" have q=1.0, in the sample above 
"text/html" will be selected.

Media parameters are supported in the header and in the cases are supported. 
If the header doesn't contain media range with parameters from case label _but_
media range _without_ parameters matches, media range from case label will get
q-value from media range with parameters. For example:

      (dispatch-mime-type "text/html;level=0.7,text/html;q=0.8,*/*;q=0.1"
        ("text/plain"
         "text/plain")
        ("text/html;level=2"
         "text/html;level=2")
        ("text/html"
         "text/html")
        ("text/html;level=1"
         "text/html;level=1")
        (otherwise
         nil))

"text/html;level=2" will be selected beacuse of inheritance of q-value from "text/html"
(0.8).
