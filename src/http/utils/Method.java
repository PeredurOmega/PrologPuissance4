package http.utils;

public enum Method {
    GET("GET"),
    POST("POST"),
    HEAD("HEAD"),
    PUT("PUT"),
    DELETE("DELETE"),
    CONNECT("CONNECT"),
    OPTIONS("OPTIONS"),
    TRACE("TRACE"),
    PATCH("PATCH"),
    UNKNOWN("UNKNOWN");

    private final String methodType;

    Method(String methodType) {
        this.methodType = methodType;
    }

    public static Method getMethod(String s) {
        switch (s) {
            case "GET":
                return Method.GET;
            case "POST":
                return Method.POST;
            case "HEAD":
                return Method.HEAD;
            case "PUT":
                return Method.PUT;
            case "DELETE":
                return Method.DELETE;
            case "CONNECT":
                return Method.CONNECT;
            case "OPTIONS":
                return Method.OPTIONS;
            case "TRACE":
                return Method.TRACE;
            case "PATCH":
                return Method.PATCH;
            case "UNKNOWN":
                return Method.UNKNOWN;
        }
        return null;
    }

    @Override
    public String toString() {
        return methodType;
    }
}
