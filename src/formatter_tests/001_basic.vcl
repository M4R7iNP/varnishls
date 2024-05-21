#  vim: tabstop=4 shiftwidth=4 expandtab:
vcl   4.0 ;

    include  "hello.vcl"; // jasså?
include  "hei.vcl";

sub  vcl_recv {
if (req.url == "lol") {
set  req.url   =   "heisann!" ; 
}


}
