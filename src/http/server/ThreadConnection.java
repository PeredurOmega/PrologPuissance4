package http.server;

import http.utils.Method;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.*;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.file.*;

public class ThreadConnection extends Thread {
    private Socket clientSocket;
    private BufferedReader in;
    private PrintWriter out;
    private InputStream input;

    ThreadConnection(Socket s) {
        clientSocket = s;
        try {
            input = clientSocket.getInputStream();
            in = new BufferedReader(new InputStreamReader(input));
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
        }

        HttpResponse response = new HttpResponse(request);
        System.out.println(response.getResponse());
        out.println(response.getResponse());
        out.flush();
        out.close();


//        if (request.getMethod() == Method.POST) {
//            String parameters = getRequestContentString();
//            System.out.println(parameters);
//            request.addParameters(parameters);
//        }
//        if (request.getMethod() == Method.PUT) {
//            String parameters = "";
//            String contentType = request.getHeaderInfo().get("Content-Type");
//            if (contentType.equals("text/html") || contentType.equals("text/plain")) {
//                parameters = getRequestContentString();
//                try {
//                    Files.writeString(Paths.get("./" + request.getUri()), parameters, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE);
//                } catch (IOException e) {
//                    e.printStackTrace();
//                }
//            } else if (contentType.equals("image/png")) {
//                createFile("./" + request.getUri(), request);
//            }
//            System.out.println(parameters);
//        }


//        //System.out.println("<img src=\"data:image/png;base64," + response.getResponseContent() + "\"/>");
//
        //out.println(response.getResponse());
        //out.println("<img src=\"data:image/png;base64," + response.getResponseContent() + "\"/>");
        //out.println("<!DOCTYPE html><html><body><img src=\"data:image/png;base64," + response.getResponseContent() + "\"/></body></html>");
    }

    private void createFile(String s, ReceivedHttpRequest r) {
        InputStream input = null;
        Path path = Paths.get(s);
        try {
            input = clientSocket.getInputStream();
            for (int i = 0; i < r.getHeaderInfo().size(); i++) {
                input.read();
            }
            Files.copy(input, path, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }


    private String readRequest() {
        StringBuilder request = new StringBuilder();
        String line;
        try {
            while (!(line = in.readLine()).trim().equals("")) {
                request.append(line + "\n");
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

    private void readBodyToImg() {
        try {
            System.out.println(input.available());

            byte[] sizeAr = new byte[4];
            input.read(sizeAr);
            int size = ByteBuffer.wrap(sizeAr).asIntBuffer().get();

            byte[] imageAr = new byte[size];
            input.read(imageAr);

            BufferedImage image = ImageIO.read(new ByteArrayInputStream(imageAr));
            System.out.println("Received " + image.getHeight() + "x" + image.getWidth() + ": " + System.currentTimeMillis());
            ImageIO.write(image, "jpg", new File("./test2.jpg"));

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

//    private String readRequest() {
//        StringBuilder request = new StringBuilder();
//        while(true) {
//            try {
//                if(!in.ready()) break;
//                request.append((char) in.read());
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }
//        return request.toString();
//    }

//    private String readRequest() {
//        StringBuilder request = new StringBuilder();
//        while(true) {
//            try {
//                if(input.available() == 0) break;
//                request.append((char) input.read());
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }
//        return request.toString();
//    }

}
