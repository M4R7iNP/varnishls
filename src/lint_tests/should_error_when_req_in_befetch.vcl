sub vcl_backend_fetch {
    set req.http.lololol = "/";
}
