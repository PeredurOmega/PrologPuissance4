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
        try{
            String str = ".";
            while (!str.equals("")) {
                try {
                    str = in.readLine();
                    request.append(str);
                    request.append("&&&");
                    System.out.println(str);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            in.readLine();
            String parameters = in.readLine();
            System.out.println(parameters);
//            for (int i = 0; i < 2; i++) {
//                try {
//                    str = in.readLine();
//                    request.append(str);
//                    request.append("&&&");
//                    System.out.println(str);
//                } catch (IOException e) {
//                    e.printStackTrace();
//                }
//            }

        } catch (Exception e) {
            System.out.println("Null mais en vrai ça passe");
        }
        ReceivedHttpRequest parsedRequest = new ReceivedHttpRequest(request.toString());
        HttpResponse response = new HttpResponse(parsedRequest);
        out.println(response.getResponse());
        out.flush();

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
