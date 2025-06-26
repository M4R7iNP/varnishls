sub vcl_recv {
    if (req.url ~ "^/api") {
        return (pass);
    } elsif (req.url ~ "^/docs") {
        return (pass);
    } elif (req.url ~ "^/.well-known") {
        return (pass);
    } elseif (req.url ~ "^/humans.txt") {
        return (pass);
    } else {
        return (pass);
    }
}
