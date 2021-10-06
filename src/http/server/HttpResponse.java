package http.server;

import http.utils.StatusCode;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.*;

public class HttpResponse {
    private ReceivedHttpRequest request;
    private String response = "";

    HttpResponse(ReceivedHttpRequest req) {
        request = req;
        buildResponse();
    }

    public String getResponse() {
        return response;
    }

    public void buildResponse() {
        String path = "./" + request.getUri();
        File resourceFile = new File(path);
        switch (request.getMethod()){
            case GET:
                if (resourceFile.exists()) {
                    createHeader(StatusCode._200, "text/html");
                    readResource(path);
                } else {
                    createHeader(StatusCode._404, "text/html");
                }
                break;
            case POST:
                if (resourceFile.exists()) {
                    createHeader(StatusCode._200, "application/json");
                    addJSONParameters();
                } else {
                    createHeader(StatusCode._404, "text/html");
                    readResource("./files/html/404.html");
                }
                break;
            case HEAD:
                if (resourceFile.exists()) {
                    createHeader(StatusCode._200, "application/json");
                } else {
                    createHeader(StatusCode._404, "text/html");
                }
                break;
            case PUT:
                break;
        }
    }

    public void readResource(String path){
        try {
            FileInputStream fileToRead = new FileInputStream(path);
            Scanner scanner = new Scanner(fileToRead);
            while (scanner.hasNextLine())
            {
                response = response + scanner.nextLine() + "\n";
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public void addJSONParameters(){
        response += "{\n";
        int i = 1;
        for(Map.Entry<String,String> s : request.getParameters().entrySet()){
            response += "\t\"" + s.getKey() + "\": \"" + s.getValue() + "\"";
            if (i != request.getParameters().entrySet().size()) response += ",";
            response += "\n";
            i++;
        }
        response += "}";
    }

    public void createHeader(StatusCode s, String type){
        response += "HTTP/1.1 "+ s.toString()+"\n";
        response += "Content-Type: "+type+"\n";
        response += "\n";
    }
}
