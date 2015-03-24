package net.ninjacat.semblance.utils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;

/**
 * I/O Utils.
 */
public final class IOUtils {
    private static final int BUFFER_SIZE = 16 * 1024;

    private IOUtils() {
    }

    /**
     * Copies input stream into output stream. This function will not close streams, it is responsibility
     * of the caller.
     *
     * @param input  Input stream.
     * @param output Output stream.
     * @throws IOException Thrown if I/O error happens.
     */
    public static void copy(final InputStream input, final OutputStream output) throws IOException {
        final byte[] buffer = new byte[BUFFER_SIZE];
        int read = 1;
        while (0 < read) {
            read = input.read(buffer);
            output.write(buffer, 0, read);
        }
        output.flush();
    }

    /**
     * Loads string from input stream. This function will not close input stream, it is responsibility of the caller.
     *
     * @param input Input stream.
     * @return String with the contents of the stream.
     * @throws IOException If I/O error happens.
     */
    public static String loadFromStream(final InputStream input) throws IOException {
        try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            copy(input, baos);
            return new String(baos.toByteArray(), Charset.forName("UTF-8"));
        }
    }
}
