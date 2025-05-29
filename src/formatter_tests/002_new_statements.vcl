import brotli;
import goto  ;

 
sub vcl_init {
new api = directors.round_robin();new  frontends = directors.round_robin();brotli.init();
}
