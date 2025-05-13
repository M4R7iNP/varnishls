; inherits: vcl

[
  "varnishtest"
  "varnish"
  "client"
  "server"
  "expect"
  "rxreq"
  "txreq"
  "rxresp"
  "txresp"
  "shell"
  "process"
  "setenv"
  "loop"
  "delay"
  "accept"
  "feature"
  "txsettings"
  "rxgoaway"
  "stream"
  "barrier"
  "haproxy"
  "syslog"
  "logexpect"
  "tls_config"
  "tls_handshake"
  "send"
  "vtest"
  "cond"
  "sock"
  "sync"
  "deplay"
  "expect_close"
] @keyword

"<undef>" @constant

[
  "-connect"
  "-proxy1"
  "-proxy2"
  "-listen"
  "-dispatch"
  "-repeat"
  "-req"
  "-vcl"
  "+backend"
  "-expect"
  "-ifunset"
  "-cyclic"
] @parameter

(argument_ident) @parameter
