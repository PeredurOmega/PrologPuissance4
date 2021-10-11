///A Simple Web Server (WebServer.java)

package http.server;

import javax.net.ssl.*;
import java.io.FileInputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.security.KeyStore;
import java.security.cert.Certificate;

/**
 * Example program from Chapter 1 Programming Spiders, Bots and Aggregators in
 * Java Copyright 2001 by Jeff Heaton
 * <p>
 * WebServer is a very simple web-server. Any request is responded with a very
 * simple web-page.
 *
 * @author Jeff Heaton
 * @version 1.0
 */
public class WebServer {
    /**
     * WebServer constructor.
     */
    protected void start(String mode) {
        String keystoreFilename;
        char[] keystorePassword;
        char[] keyPassword;
        String keyAlias;
        KeyStore keyStore;
        ServerSocket serverSocket = null;

        try {
            if (mode.equals("http")) {
                serverSocket = new ServerSocket(3000);
            } else if (mode.equals("https")) {
                // Génération du certificat auto-signé :
                // Dans un terminal : keytool -genkey -keyalg RSA -alias server -keystore selfsigned.jks -validity 800 -keysize 2048

                // Vérifier que le certificat a bien été généré :
                // Dans un terminal : keytool -list -v -keystore selfsigned.jks

                // Chargement du certificat
                keystoreFilename = "selfsigned.jks";
                keystorePassword = "insaifa".toCharArray();
                keyPassword = "insaifa".toCharArray();
                keyAlias = "server";
                keyStore = KeyStore.getInstance("JKS");
                keyStore.load(new FileInputStream(keystoreFilename), keystorePassword);

                // Affichage du certificat
                Certificate certificate = keyStore.getCertificate(keyAlias);
                System.out.println(certificate);

                // Mise en place d'une KeyManageFactory
                KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
                keyManagerFactory.init(keyStore, keyPassword);

                // Mise en place d'un TrustManagerFactory
                TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance("SunX509");
                trustManagerFactory.init(keyStore);

                // Mise en place du contexte SSL
                SSLContext sslContext = SSLContext.getInstance("TLS");
                sslContext.init(keyManagerFactory.getKeyManagers(), null, null);

                // Création d'une SSLServerSocketFactory serverSocket'appuyant sur le protocole SSL défini
                SSLServerSocketFactory sslServerSocketFactory = sslContext.getServerSocketFactory();

                // Création du ServerSocket serverSocket'appuyant sur le protocole SSL défini à l'aide de la SSLServerSocketFactory
                serverSocket = (SSLServerSocket) sslServerSocketFactory.createServerSocket(3000);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.out.println("Webserver starting up on port 80");
        System.out.println("(press ctrl-c to exit)");
        try {

            System.out.println("Server started:");
            printServerSocketInfo(serverSocket);
        } catch (Exception e) {
            System.out.println("Error: " + e);
            return;
        }

        System.out.println("Waiting for connection");
        for (; ; ) {
            try {
                Socket remote = null;
                // wait for a connection
                if (mode.equals("http")) {
                    remote = serverSocket.accept();
                } else if (mode.equals("https")) {
                    remote = (SSLSocket) serverSocket.accept();
                }
                // remote is now the connected socket
                System.out.println("Connection, sending data.");
                printClientSocketInfo(remote);

                ThreadConnection t = new ThreadConnection(remote);
                t.start();
            } catch (Exception e) {
                System.out.println("Error: " + e);
            }
        }
    }

    private static void printClientSocketInfo(Socket socket) {
        System.out.println("Client Socket class: " + socket.getClass());
        System.out.println("\tRemote address = " + socket.getInetAddress().toString());
        System.out.println("\tRemote port = " + socket.getPort());
        System.out.println("\tLocal socket address = " + socket.getLocalSocketAddress().toString());
        System.out.println("\tLocal address = " + socket.getLocalAddress().toString());
        System.out.println("\tLocal port = " + socket.getLocalPort());
        if (socket instanceof SSLSocket) {
            SSLSocket sslSocket = (SSLSocket) socket;
            System.out.println("\tNeed client authentication = " + sslSocket.getNeedClientAuth());
            SSLSession sslSession = sslSocket.getSession();
            System.out.println("\tCipher suite = " + sslSession.getCipherSuite());
            System.out.println("\tProtocol = " + sslSession.getProtocol());
        }
    }

    private static void printServerSocketInfo(ServerSocket socket) {
        System.out.println("Server socket class: " + socket.getClass());
        System.out.println("\tSocket address = " + socket.getInetAddress().toString());
        System.out.println("\tSocket port = " + socket.getLocalPort());
        if (socket instanceof SSLServerSocket) {
            SSLServerSocket sslServerSocket = (SSLServerSocket) socket;
            System.out.println("\tNeed client authentication = " + sslServerSocket.getNeedClientAuth());
            System.out.println("\tWant client authentication = " + sslServerSocket.getWantClientAuth());
            System.out.println("\tUse client mode = " + sslServerSocket.getUseClientMode());
        }
    }

    /**
     * Start the application.
     *
     * @param args Command line parameters are not used.
     */
    public static void main(String args[]) {
        String mode = args[0];
        if (!mode.equals("http") && !mode.equals("https")) {
            return;
        }
        WebServer ws = new WebServer();
        ws.start(mode);
    }
}
