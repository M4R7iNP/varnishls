sub vcl_deliver {
    unset resp.http.Access-Control-Allow-Origin;
    set resp.http.Age = 0;
    if (req.http.X-Forwarded-For != "::1") {
        set resp.http.Vary = resp.http.Vary + ", origin";
        set resp.http.X-Powered-By = "Varnish :)";
        set resp.http.Via = "Varnish :)";
    }
}
