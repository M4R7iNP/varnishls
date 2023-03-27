vcl 4.0;

import accept;
import std;
import var;

probe localhost_probe {
    .url = "/_health";
    .interval = 5s;
    .timeout = 3.14s;
    .threshold = 3;
}

backend localhost {
    .host = "localhost";
    .port = "8080";
    .probe = localhost_probe;
}

sub vcl_init {
    var.global_set("hello", "world");

    new jwt_reader = jwt.reader();

    if (var.global_get("hello") == "darkness") {
        std.syslog(3, "my old friend");
        return(fail);
    }
}

sub add_cors_header {
    if (req.http.origin ~ "https?://darthvader.no") {
        set req.http.access-control-allow-origin = req.http.origin;
    }
}

acl acl_purge {
    "localhost";
    "10.0.0.0/8"; // hello
    "192.168.0.0"/16; // world
}

sub vcl_recv {
    if (req.restarts == 0) {
        // httpoxy
        unset req.http.proxy;
    }

    if (req.method == "PURGE") {
        if (client.ip !~ acl_purge || !jwt_reader.parse(req.http.Authorization)) {
            return(synth(405, "Could not purge"));
        }

        return(hash);
    }

    if (req.url == "/_health") {
        return(synth(200, "ok"));
    }

    if (req.url ~ "^/api") {
        set req.backend_hint = localhost;
        call add_cors_header;
        return(pass);
    }
}
