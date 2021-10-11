package http.utils;

import java.util.ArrayList;
import java.util.List;

public enum ContentType {
    text_plain("text/plain"),
    text_html("text/html"),
    text_css("text/css"),
    text_javascript("text/javascript"),

    image_gif("image/gif"),
    image_png("image/png"),
    image_jpeg("image/jpeg"),
    image_bmp("image/bmp"),
    image_webp("image_webp"),

    audio_midi("audio/midi"),
    audio_mpeg("audio/mpeg"),
    audio_webm("audio/webm"),
    audio_ogg("audio/ogg"),
    audio_wav("audio/wav"),
    audio_mp4("audio/mp4"),

    video_webm("video/webm"),
    video_ogg("video/ogg"),
    video_mpeg("video/mpeg"),
    video_mp4("video/mp4"),
    video_quicktime("video/quicktime"),

    application_octet_stream("application/octet-stream"),
    application_pkcs12("application/pkcs12"),
    application_vnd_mspowerpoint("application/vnd.mspowerpoint"),
    application_xhtml_xml("application/xhtml+xml"),
    application_xml("application/xml"),
    application_pdf("application/pdf");

    private final String contentType;

    ContentType(String contentType) {
        this.contentType = contentType;
    }

    public String getContentType() {
        return contentType;
    }

    public static boolean isTextContentType(String contentType) {
        List<String> textContentTypes = new ArrayList<String>();
        textContentTypes.add(ContentType.text_plain.getContentType());
        textContentTypes.add(ContentType.text_html.getContentType());
        textContentTypes.add(ContentType.text_css.getContentType());
        textContentTypes.add(ContentType.text_javascript.getContentType());
        return textContentTypes.contains(contentType);
    }

    public static boolean isImageContentType(String contentType) {
        List<String> textContentTypes = new ArrayList<String>();
        textContentTypes.add(ContentType.image_gif.getContentType());
        textContentTypes.add(ContentType.image_png.getContentType());
        textContentTypes.add(ContentType.image_jpeg.getContentType());
        textContentTypes.add(ContentType.image_bmp.getContentType());
        textContentTypes.add(ContentType.image_webp.getContentType());
        return textContentTypes.contains(contentType);
    }

    public static boolean isAudioContentType(String contentType) {
        List<String> textContentTypes = new ArrayList<String>();
        textContentTypes.add(ContentType.audio_midi.getContentType());
        textContentTypes.add(ContentType.audio_mpeg.getContentType());
        textContentTypes.add(ContentType.audio_webm.getContentType());
        textContentTypes.add(ContentType.audio_ogg.getContentType());
        textContentTypes.add(ContentType.audio_wav.getContentType());
        textContentTypes.add(ContentType.audio_mp4.getContentType());
        return textContentTypes.contains(contentType);
    }
    public static boolean isVideoContentType(String contentType) {
        List<String> textContentTypes = new ArrayList<String>();
        textContentTypes.add(ContentType.video_webm.getContentType());
        textContentTypes.add(ContentType.video_ogg.getContentType());
        textContentTypes.add(ContentType.video_mpeg.getContentType());
        textContentTypes.add(ContentType.video_mp4.getContentType());
        textContentTypes.add(ContentType.video_quicktime.getContentType());
        return textContentTypes.contains(contentType);
    }

    public static boolean isApplicationContentType(String contentType) {
        List<String> textContentTypes = new ArrayList<String>();
        textContentTypes.add(ContentType.application_octet_stream.getContentType());
        textContentTypes.add(ContentType.application_pkcs12.getContentType());
        textContentTypes.add(ContentType.application_vnd_mspowerpoint.getContentType());
        textContentTypes.add(ContentType.application_xhtml_xml.getContentType());
        textContentTypes.add(ContentType.application_xml.getContentType());
        textContentTypes.add(ContentType.application_pdf.getContentType());
        return textContentTypes.contains(contentType);
    }
    @Override
    public String toString() {
        return contentType;
    }
}
