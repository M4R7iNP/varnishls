sub vcl_deliver {
    set resp.http.Location = "/";
    set resp.http.Access-Control-Allow-Origin = "*";
}
