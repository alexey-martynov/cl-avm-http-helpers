cl-avm-http-helpers
===================

This project contains different utilities to handle some tricky aspects
of the HTTP protocol.

Also it contains helper to publish templates compiled via
[cl-closure-template](https://github.com/archimag/cl-closure-template).

> IMPORTANT NOTE:
> Some library parts are accidentally bound to the [Hunchentoot](http://weitz.de/hunchentoot/).
> This should be fixed via providing some generic mechanism or Hunchentoot
> availability detection.

HTTP Headers
------------

At this time library provides handling of the `Accept` header and
conditional execution with `Last-Modified`/`If-Modified-Since`/`If-Unmodified-Since`.

### `Accept` Header

A typical usage looks like:

    (dispatch-mime-type
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

When `Accept` header contains "text/html;level=0.7,text/html;q=0.8,*/*;q=0.1"

    (dispatch-mime-type
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

"text/html;level=2" will be selected because of inheritance of q-value from "text/html"
(0.8).

There are 2 versions of the macros is available. `dispatch-mime-type` automatically selects
HTTP server implementation. `dispatch-mime-type*` accepts additional argument with the HTTP
implementation identifier.

If it is possible for clients to receive different MIME types with the (for example, image
as PNG to display initial picture or image as BASE64 for Ajax/Web 2.0 application to update
it), the header `Vary` *must* be set to at least `Accept`. This informs all caches that
content depends on the supplied `Accept` header. The macros perform this task automatically.
But if handling code requires additional data in the `Vary` header it should set it manually
with `Accept` inside.

### Conditional Processing

HTTP 1.1 offers mechanism to control caching. In the simplest version it consists of
`Last-Modified` response header and `If-Modified-Since`/`If-Unmodified-Since` request
headers.

The library provides 2 pairs of macros: `when-modified`/`when-modified*` and
`unless-modified`/`unless-modified*` based on timestamps.

The macros `when-modified*` and `unless-modified*` are generic ones. They receive
resource time stamp and implementation identifier.

The macros `when-modified` and `unless-modified` are designed for regular usage. They
receive resource time stamp as Common Lisp universal time. The body of the macro invocation
is executed if comparison of the time stamp to the header's value is succeeded. The
implementation is auto-detected. The first one is from `*default-http-implementation*` variable.
If it is NIL the `*features*` is checked for Hunchentoot.

If the body of the macro is executed, the header `Last-Modified` is set to the representation
of the provided time stamp.

For example:

    (when-modified (file-write-date path)
      (generate-contents path))

Additional facility provided for ETag based conditions. The macros
`when-matched`/`when-matched*` handles conditions in `If-Match` header
accodirding to section 14.24 of RFC 2616. For example:

    (when-match (etag-value)
      (generate-response))

The folliowing cases are possible:

1. `If-Match` and `etag-value` are missing, the handler is
   evaluated.
2. `If-Match` and `etag-value` are exists and match, the handler is
   evaluated.
3. `If-Match` and `etag-value` are exists and but don't match, the
   handler is not evaluated and "412 Percondition Failed" is
   returned.
4. `If-Match` exists but `etag-value` is missing. This case is handled
   as "Resource Not Found" and "404 Not Found" returned except for
   `If-Match` value "*" which is stated in RFC. In this case "412
   Precondition Failed" is returned. The handler body is not
   evaluated.
5. `If-Match` is missing but `etag-value` exists. The handler body is
   not evaluated and "428 Precondition Required" is returned.

To distinguish case 1 and 2 inside the handler body additional
lexical variable is bound during body evaluation. By default it is
named `etag-matched` but other name can be specified as a second
parameter after ETag value.

Another pair of macros `unless-matched`/`unless-matched*` processes
opposite condition. The cases are processed in the following ways:

1. `If-None-Match` and `etag-value` are missing, the handler is
   evaluated.
2. `If-None-Match` and `etag-value` are exists and match, the handler is
   not evaluated and "412 Precondition Failed" is returned in all
   cases except for GET requests. For GET requests "304 Not Modified"
   is returned (RFC, section 14.26).
3. `If-None-Match` and `etag-value` are exists and but don't match, the
   handler is evaluated.
4. `If-None-Match` exists but `etag-value` is missing. This case is
   handled as "Resource Not Found" and "404 Not Found" returned. The
   handler body is not evaluated.
5. `If-None-Match` is missing but `etag-value` exists. The handler body is
   evaluated as usual.

To simplify setting `ETag` header function `set-etag` performs quoting
and prefixing. If `WEAK` parameter is non-`NIL` value is perfixed with
"W/" to designate weak e-tag. Server implementation is selected via
`IMPLEMENTATION` parameter.

PATCH Method Handling
---------------------

The PATCH method is designed to apply list of "patches" to a
resource. The macro `handle-patch-actions` created to simplify
creation of such request handlers. It has a form

    (handle-patch-actions (request &optional object)
      ("action-1-name" lambda-1)
      ("action-2-name" lambda-2)
      ...)

The overall design is created to simplify handling of JSON array of
actions. So the following rules apply:

* `request` is a list of actions.
* Each action is an alist which contains string value with key
  `:action`. This value identifies handler.
* The rest of action's alist is a set of parameters for handler.
* `lambda-x` is a function suitable to be passed to `funcall`.
* It should be callable as `(lambda-x object action)`. `object` is the
  data to patch, `action` is the action's alist.
* It should return patched object. It is up to implementor whether new
  object returned or updated parameter.
* Actions invoked one by one in order in `request`.
* The first action takes `object` value as object parameter, all other
  actions will take result of the previous action.
* Overall result is the result of the last action.

Please note that the order of parameter evaluation is not guaranteed
to be "from left to right".

The following conditions raised:

* `invalid-action-specifier` when missing `:action` key or action name
  is not string.

* `action-handler-not-found` when unknown action received.

The restarts `ignore-action` and `use-value` are provided in case of
conditions above. The restart `ignore-action` continues processing
`request` ignoring erroneous one. The object parameter for the next
action will be taken form result of the previous one. `use-value`
restart gives ability to replace call to incorrect action with
specified value.

Hunchentoot Redirector
----------------------

`cl-avm-http-helpers.hunchentoot:redirect-acceptor` is a special
acceptor class to perform, for example, redirection from HTTP URLs to
HTTPS. The host and URI part are preserved during redirection. This
acceptor has the followinf additional parameters to perform its task:

* `target-port` - a port number to redirect to in range
  1..65535. Required.
* `target-protocol` - a protocol to redirect to. Supported values are
  `:http` and `:https` with `:https` default.

Closure Template Publishing
---------------------------

Templates are published via [RESTAS](https://github.com/archimag/restas) after
compilation to the [RequireJS](http://requirejs.org/) module format.

> IMPORTANT NOTE:
> This library is bound to the Hunchentoot server implementation because of RESTAS usage.

Component is implemented in `cl-avm-requirejs-publisher` system and can be used as module.

For example:

    (restas:mount-module symbol (#:cl-avm-requirejs-publisher)
      (cl-avm-requirejs-publisher:*source-dir* #p"/path/to/templates/"))

Usable parameters:
- `*source-dir*` - directory that contains templates to be published; required;
- `*extension*` - template file extension which replaces "js" extension in request,
  by default "tmpl".
