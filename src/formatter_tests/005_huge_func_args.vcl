sub set_cookie {
header.append(
beresp.http.Set-Cookie, "my_cookie=" +
bereq.http.X-MY-COOKIE + "; domain=" + bereq.http.host + ";max-age=" + bereq.http.X-MYCOOKIE-Age + "; path=/")
}
