package http.utils;

public enum StatusCode {

    // Information Codes
    _100("100 Continue"),
    _101("101 Switching Protocols"),
    _102("102 Processing"),
    _103("103 Early Hints"),

    // Success Codes
    _200("200 OK"),
    _201("201 Created"),
    _202("202 Accepted"),
    _203("203 Non-Authoritative Information"),
    _204("204 No Content"),
    _205("205 Reset Content"),
    _206("206 Partial Content"),
    _207("207 Mutli-Status"),
    _208("208 Already reported"),
    _210("210 Content Different"),
    _226("226 IM Used"),

    // Redirection Codes
    _300("300 Multiple Choices"),
    _301("301 Moved Permanently"),
    _302("302 Found"),
    _303("303 See Other"),
    _304("304 Not Modified"),
    _305("305 Use Proxy"),
    _306("Switch Proxy"),
    _307("307 Temporary Redirect"),
    _308("308 Permanent Redirect"),
    _310("310 Too many Redirects"),

    // Errors of HTTP Client Codes
    _400("400 Bad Request"),
    _401("401 Unauthorized"),
    _402("402 Payment Required"),
    _403("403 Forbidden"),
    _404("404 Not Found"),
    _405("405 Method Not Allowed"),
    _406("406 Not Acceptable"),
    _407("407 Proxy Authentication Required"),
    _408("408 Request Time-out"),
    _409("409 Conflict"),
    _410("410 Gone"),
    _411("411 Length Required"),
    _412("412 Precondition Failed"),
    _413("413 Request Entity Too Large"),
    _414("414 Request-URI Too Large"),
    _415("415 Unsupported Media Type"),
    _416("416 Requested range not satisfiable"),
    _417("417 Expectation Failed"),
    _418("418 I’m a teapot"),
    _421("421 Bad mapping / Misdirected Request"),
    _422("422 Unprocessable entity"),
    _423("423 Locked WebDAV"),
    _424("424 Method failure"),
    _425("425 Too Early"),
    _426("426 Upgrade Required"),
    _428("428 Precondition Required"),
    _429("429 Too Many Requests"),
    _431("431 Request Header Fields Too Large"),
    _449("449 Retry With"),
    _450("450 Blocked by Windows Parental Controls"),
    _451("451 Unavailable For Legal Reasons"),
    _456("456 Unrecoverable Error"),

    // Errors of HTTP Server Codes
    _500("500 Internal Server Error"),
    _501("501 Not Implemented"),
    _502("502 Bad Gateway"),
    _503("503 Service Unavailable"),
    _504("504 Gateway Time-out"),
    _505("505 HTTP Version not supported"),
    _506("506 Variant Also Negotiates"),
    _507("507 Insufficient storage"),
    _508("508 Loop detected"),
    _509("509 Bandwidth Limit Exceeded"),
    _510("510 Not extended"),
    _511("511 Network authentication required");

    private final String statusCode;

    StatusCode(String statusCode) {
        this.statusCode = statusCode;
    }
}
