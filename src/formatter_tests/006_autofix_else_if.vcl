sub vcl_recv {
    if (req.url ~ "^/a/") {
        call a;
    } elsif (req.url ~ "^/b/") {
        call b;
    } elseif (req.url ~ "^/c/") {
        call c;
    } else if (req.url ~ "^/d/") {
        call d;
    } elif (req.url ~ "^/e/") {
        call e;
    } else {
        call f;
    }

}
