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

    @Override
    public String toString() {
        return methodType;
    }
}
