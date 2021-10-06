package http.server;

import http.utils.StatusCode;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

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
        switch (request.getMethod()){
            case GET:
                String path = "./" + request.getUri();
                File resourceFile = new File(path);
                if (resourceFile.exists()) {
                    createHeader(StatusCode._200);
                    readResource(path);
                } else {
                    createHeader(StatusCode._404);
                    readResource("./files/html/404.html");
                }
                break;
            case POST:

                break;
            case HEAD:
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

    public void createHeader(StatusCode s){
        response += "HTTP/1.1 "+ s.toString()+"\n";
        response += "Content-Type: text/html\n";
        response += "\n";
    }
}
