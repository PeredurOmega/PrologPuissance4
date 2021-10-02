package http.server;

import http.utils.Method;

import java.util.ArrayList;
import java.util.List;

public class ReceivedHttpRequest {
    List<String> headerInfo = new ArrayList<String>();

    Method method;
    String uri;
    String version;

    ReceivedHttpRequest() {
        //TODO parse HTTP request from client to build the object
    }


}
