package net.ninjacat.semblance.reader;

import org.junit.Test;

import java.io.StringReader;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class TokenizerTest {

    @Test
    public void testSimpleTokenizationOfText() throws Exception {
        final Tokenizer tokenizer = createTokenizer("Text number(123)");

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.WORD));
        assertThat(tokenizer.getTokenValue(), is("Text"));

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.WORD));
        assertThat(tokenizer.getTokenValue(), is("number"));

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.SEPARATOR));
        assertThat(tokenizer.getTokenValue(), is("("));

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.WORD));
        assertThat(tokenizer.getTokenValue(), is("123"));

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.SEPARATOR));
        assertThat(tokenizer.getTokenValue(), is(")"));

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.EOF));
        assertThat(tokenizer.getTokenValue(), is(""));
    }

    @Test
    public void testShouldTokenizeDoublesCorrectly() throws Exception {
        final Tokenizer tokenizer = createTokenizer("123.43e-12");

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.WORD));
        assertThat(tokenizer.getTokenValue(), is("123.43e-12"));

        assertThat(tokenizer.nextToken(), is(Tokenizer.TextType.EOF));
        assertThat(tokenizer.getTokenValue(), is(""));
    }

    private Tokenizer createTokenizer(final String text) {
        return new Tokenizer(new StringReader(text));
    }
}