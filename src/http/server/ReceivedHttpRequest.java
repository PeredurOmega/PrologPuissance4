package http.server;

import http.utils.Method;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ReceivedHttpRequest {
    HashMap<String, String> headerInfo = new HashMap();

    private Method method;
    private String uri;
    private String version;

    ReceivedHttpRequest(String r) {

    }

    public HashMap<String, String> getHeaderInfo() {
        return headerInfo;
    }

    public Method getMethod() {
        return method;
    }

    public String getUri() {
        return uri;
    }

    public String getVersion() {
        return version;
    }

    public void parseRequest(String s){
        String [] parsedElements = s.split("&&&");
        String [] mainElement = parsedElements[0].split(" ");
        method = Method.getMethod(mainElement[0]);
        uri = mainElement[1];
        version = mainElement[2];
        for (int i = 1; i<parsedElements.length; i++){
            String [] splitted = parsedElements[i].split(": ");
            headerInfo.put(splitted[0], splitted[1]);
        }
    }

}
