package http.server;

import http.utils.Method;

import java.io.*;
import java.net.Socket;

public class ThreadConnection extends Thread {
    private BufferedReader in;
    private PrintWriter out;

    ThreadConnection(Socket s) {
        try {
            in = new BufferedReader(new InputStreamReader(s.getInputStream()));
            out = new PrintWriter(s.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void run() {
        String requestString = readRequest();
        ReceivedHttpRequest request = new ReceivedHttpRequest(requestString);

        if (request.getMethod() == Method.POST || request.getMethod() == Method.PUT) {
            request.setBody(readBodyToString());
//            readRequestBody(request);
        }

        HttpResponse response = new HttpResponse(request);
//        System.out.println("Réponse : " + response.getResponse());
        out.println(response.getResponse());
        out.flush();
        out.close();
    }

    private String readRequest() {
        StringBuilder request = new StringBuilder();
        String line;

        try {
            while (!(line = in.readLine()).trim().equals("")) {
                request.append(line).append("\n");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return request.toString();
    }

    private String readBodyToString() {
        StringBuilder request = new StringBuilder();
        try {
            while (true) {
                try {
                    if (!in.ready()) break;
                    request.append((char) in.read());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return request.toString();
    }

//    private String wesh(ReceivedHttpRequest request) {
//        int len = Integer.parseInt(request.getHeaderInfo().get("Content-Length"));
//        char[] readed = new char[len];
//        try {
//            in.read(readed, 0, len);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//        return new String(readed, StandardCharsets.UTF_8);
//    }

    private String readRequestBody(ReceivedHttpRequest request) {
        StringBuilder requestString = new StringBuilder();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int len = Integer.parseInt(request.getHeaderInfo().get("Content-Length"));

        for (int i = 0; i < len; i++ ) {
            try {
                if(!in.ready()) break;
                byte bit = (byte) in.read();
                baos.write(new byte[]{bit}, 0, 1);

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        try(OutputStream outputStream = new FileOutputStream("image.png")) {
            baos.writeTo(outputStream);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return requestString.toString();
    }
}
