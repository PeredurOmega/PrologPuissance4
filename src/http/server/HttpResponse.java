package http.server;

import http.utils.ContentType;
import http.utils.StatusCode;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Base64;
import java.util.Map;

public class HttpResponse {
    private final ReceivedHttpRequest request;
    private String response = "";
    private byte[] responseContent;

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
        if (resourceFile.isDirectory()) {
            createHeader(StatusCode._404, "text/html");
            readResource("./files/text/404.html");
            formatTextReponse();
        } else {
            switch (request.getMethod()) {
                case GET:
                    if (resourceFile.exists()) {
                        createHeader(StatusCode._200, request.getContentType());
                        readResource(path);
                        formatReponse();
                    } else {
                        createHeader(StatusCode._404, "text/html");
                        readResource("./files/text/404.html");
                        formatTextReponse();
                    }
                    break;
                case POST:
                    request.addParameters();
                    if (resourceFile.exists()) {
                        createHeader(StatusCode._200, "application/json");
                        addJSONParameters();
                    } else {
                        createHeader(StatusCode._404, "text/html");
                        readResource("./files/text/404.html");
                        formatTextReponse();
                    }
                    break;
                case HEAD:
                    if (resourceFile.exists()) {
                        createHeader(StatusCode._200, request.getContentType());
                    } else {
                        createHeader(StatusCode._404, "text/html");
                        readResource("./files/text/404.html");
                        formatTextReponse();
                    }
                    break;
                case PUT:
                    if (ContentType.isTextContentType(request.getContentType())) {
                        try {
                            Files.writeString(Paths.get("./" + request.getUri()), request.getBody(), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE);
                            createHeader(StatusCode._201, "text/html");
                            readResource("./files/text/added.html");
                            formatTextReponse();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    } else {
                        createHeader(StatusCode._415, "text/html");
                        readResource("./files/text/impossible.html");
                        formatTextReponse();
                    }
                    break;
                case DELETE:
                    if (resourceFile.delete()) {
                        createHeader(StatusCode._200, "text/html");
                        readResource("./files/text/delete.html");
                        formatTextReponse();
                    } else {
                        createHeader(StatusCode._404, "text/html");
                        readResource("./files/text/404.html");
                        formatTextReponse();
                    }
                    break;
            }
        }
    }

    private void readResource(String path) {
        byte[] content = null;
        try {
            content = Files.readAllBytes(Paths.get(path));
        } catch (IOException e) {
            e.printStackTrace();
        }
        responseContent = content;
    }

    public void formatReponse() {
        System.out.println(request.getContentType());
        if (ContentType.isTextContentType(request.getContentType())) {
            formatTextReponse();
        } else if (ContentType.isImageContentType(request.getContentType())){
            formatImageResponse();
        } else if (request.getContentType().equals("application/pdf")){
            formatPDFResponse();
        } else if (ContentType.isVideoContentType(request.getContentType())) {
            formatVideoResponse();
        } else if (ContentType.isAudioContentType(request.getContentType())) {
            formatAudioResponse();
        }
    }

    public void formatTextReponse() {
        response += new String(responseContent, StandardCharsets.UTF_8);
    }

    public void formatAudioResponse() {
        response += "<audio controls src=\"data:"+ request.getContentType() + ";base64," + Base64.getEncoder().encodeToString(responseContent) + "\"/>";
    }

    public void formatVideoResponse() {
        response += "<video style=\"position: absolute; top: 15%; margin-left: 25%; margin-right: auto;\" width=\"1000\" controls>";
        response += "<source src=\"data:"+ request.getContentType() + ";base64," + Base64.getEncoder().encodeToString(responseContent) + "\" type=\"" + request.getContentType() + "\" />";
        response += "</video>";
    }

    public void formatPDFResponse() {
        response += "<body style=\"margin-top: 0px; margin-left: 0px; margin-right: 0px; margin-bottom: 0px;\">";
        response += "<object width=\"100%\" height=\"100%\" data=\"data:application/pdf;base64," +
                Base64.getEncoder().encodeToString(responseContent) + "\" type=\"application/pdf\" class=\"internal\">";
        response += "<embed data=\"data:application/pdf;base64," + Base64.getEncoder().encodeToString(responseContent) + " \" type=\"application/pdf\"/>";
        response += "</object>";
        response += "</body>";
    }

    public void formatImageResponse() {
        response += "<body>";
        response += "<object style=\"\n" +
                "  position: absolute;\n" +
                "  left: 45%;\n" +
                "  top: 35%;\n" +
                "  margin-left: -10px;\n" +
                "  margin-top: -10px;\" data=\"data:" + request.getContentType() + ";base64," +
                Base64.getEncoder().encodeToString(responseContent) + "\" type=\"" + request.getContentType() + "\" class=\"internal\">";
        response += "<img src=\"data:" + request.getContentType() + ";base64," + Base64.getEncoder().encodeToString(responseContent) + "\"/>";
        response += "</object>";
        response += "</body>";
    }

    public void addJSONParameters() {
        response += "{\n";
        int i = 1;
        for (Map.Entry<String, String> s : request.getParameters().entrySet()) {
            response += "\t\"" + s.getKey() + "\": \"" + s.getValue() + "\"";
            if (i != request.getParameters().entrySet().size()) response += ",";
            response += "\n";
            i++;
        }
        response += "}";
    }

    public void createHeader(StatusCode s, String type) throws NullPointerException {
        try {
            response += "HTTP/1.1 " + s.toString() + "\n";
            if (!type.equals("")) {
                response += "Content-Type: " + type + "\n";
                response += "\n";
            }
        } catch (Exception ignored) {

        }
    }
}
