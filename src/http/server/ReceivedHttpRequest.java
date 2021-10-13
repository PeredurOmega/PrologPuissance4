package http.server;

import http.utils.Method;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;

public class ReceivedHttpRequest {
    HashMap<String, String> headerInfo = new HashMap();

    private Method method;
    private String uri;
    private String contentType;
    private String version;
    private String body;
    private HashMap<String, String> parameters = new HashMap<>();


    ReceivedHttpRequest(String request) {
        parseRequest(request);
        String path = "./" + this.uri;
        try {
            contentType = Files.probeContentType(Paths.get(path));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
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

    public String getContentType() {
        return contentType;
    }

    public String getUri() {
        return uri;
    }

    public void parseRequest(String s) {
        try {
            System.out.println("Requête : " + s);
            int i;
            String[] parsedElements = s.split("\n");
            String[] mainElements = parsedElements[0].split(" ");
            method = Method.getMethod(mainElements[0]);
            uri = mainElements[1];
            version = mainElements[2];
            for (i = 1; i < parsedElements.length; i++) {
                String[] splitted = parsedElements[i].split(": ");
                headerInfo.put(splitted[0], splitted[1]);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void addParameters() {
        String[] parameters = this.body.split("&");
        for (String param : parameters) {
            String[] paramInfo = param.split("=");
            this.parameters.put(paramInfo[0], paramInfo[1]);
        }
    }

}
