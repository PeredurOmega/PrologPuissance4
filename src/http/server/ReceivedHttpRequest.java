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
    private HashMap<String,String> parameters = new HashMap<>();

    ReceivedHttpRequest(String r) {
        parseRequest(r);
    }

    ReceivedHttpRequest() {
    }

    public HashMap<String, String> getParameters() {
        return parameters;
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
        System.out.println(mainElement[1]);
        method = Method.getMethod(mainElement[0]);
        uri = mainElement[1];
        version = mainElement[2];
        for (int i = 1; i<parsedElements.length; i++){
            String [] splitted = parsedElements[i].split(": ");
            headerInfo.put(splitted[0], splitted[1]);
        }
    }

    public void addHeader(String h){
        String [] splitted = h.split(": ");
        headerInfo.put(splitted[0], splitted[1]);
    }

    public void parseFirstLine(String s){
        String [] mainElement = s.split(" ");
        method = Method.getMethod(mainElement[0]);
        uri = mainElement[1];
        version = mainElement[2];
    }

    public void addParameters(String params){
        String [] parsedParams = params.split("&");
        for (String parsedParam : parsedParams) {
            String[] param = parsedParam.split("=");
            parameters.put(param[0], param[1]);
        }
    }

}
