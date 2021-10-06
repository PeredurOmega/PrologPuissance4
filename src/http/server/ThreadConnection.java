package http.server;

import http.utils.Method;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.sql.SQLOutput;

public class ThreadConnection extends Thread{
    private Socket clientSocket;
    private BufferedReader in;
    private PrintWriter out;

    ThreadConnection(Socket s){
        clientSocket = s;
        try {
            in = new BufferedReader(new InputStreamReader(s.getInputStream()));
            out = new PrintWriter(s.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void run(){
        StringBuilder request = new StringBuilder();
        ReceivedHttpRequest parsedRequest = new ReceivedHttpRequest();
        int count = 0;
        try {
            System.out.println(in.ready());
        } catch (IOException e) {
            e.printStackTrace();
        }
        try{
            String str =".";
            while (!str.equals("")) {
                try {
                    str = in.readLine();
                    System.out.println(str);
                    if (str.equals("")){
                        if (parsedRequest.getMethod() == Method.POST){
                            int contentLen = Integer.parseInt(parsedRequest.getHeaderInfo().get("Content-Length"));
                            char[] parameters = new char[contentLen];
                            in.read(parameters,0,contentLen);
                            System.out.println(parameters);
                        }
                    } else {
                        if (count == 0) parsedRequest.parseFirstLine(str);
                        else {
                            parsedRequest.addHeader(str);
                        }
                        count++;
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            HttpResponse response = new HttpResponse(parsedRequest);
            out.println(response.getResponse());
            out.flush();
        } catch (Exception e) {
            System.out.println("Null mais en vrai ça passe");
        }

//        out.println("HTTP/1.0 200 OK\nContent-Type: text/html\nServer: Bot\n\n<H1>Welcome to the Ultra Mini-WebServer</H2>");
//        out.println("Content-Type: text/html");
//        out.println("Server: Bot");
//        // this blank line signals the end of the headers
//        out.println("");
//        // Send the HTML page
//        out.println("<H1>Welcome to the Ultra Mini-WebServer</H2>");
//        out.flush();
        try {
            clientSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
