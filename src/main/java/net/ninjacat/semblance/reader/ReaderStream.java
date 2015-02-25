package net.ninjacat.semblance.reader;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

/**
 * Created on 25/02/15.
 */
public class ReaderStream {

    private final InputStreamReader reader;
    private final Queue<Token> tokenBuffer;

    private final Set<Character> specials;

    private ReaderStream(InputStream inputStream) {
        this.reader = new InputStreamReader(new BufferedInputStream(inputStream));
        tokenBuffer = new LinkedList<>();
        specials = new HashSet<>();
    }

    public static ReaderStream readStream(InputStream inputStream) {
        return new ReaderStream(inputStream);
    }

    public static ReaderStream readString(String string) {
        return new ReaderStream(new ByteArrayInputStream(string.getBytes(Charset.forName("utf-8"))));
    }

    public void registerSpecial(char specialCharacter) {
        specials.add(specialCharacter);
    }

    public Token nextToken() {
        if (!tokenBuffer.isEmpty()) {
            return tokenBuffer.remove();
        } else {
            return scanForNextToken();
        }
    }

    private Token scanForNextToken() {
        return null;
    }

}
