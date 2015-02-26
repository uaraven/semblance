package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.errors.UnterminatedStringException;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class ReaderStreamTest {

    @Test
    public void shouldTokenizeSymbol() throws Exception {
        ReaderStream stream = ReaderStream.readString("symbol");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be symbol", tokens.get(0).getType(), is(Token.TokenType.Symbol));
        assertThat("Token value should be 'symbol'", tokens.get(0).getValue(), is("symbol"));
    }

    @Test
    public void shouldTokenizeNumber() throws Exception {
        ReaderStream stream = ReaderStream.readString("42");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be integer", tokens.get(0).getType(), is(Token.TokenType.Integer));
        assertThat("Token value should be '42'", tokens.get(0).getValue(), is("42"));
    }

    @Test
    public void shouldTokenizeNegativeNumber() throws Exception {
        ReaderStream stream = ReaderStream.readString("-42");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be integer", tokens.get(0).getType(), is(Token.TokenType.Integer));
        assertThat("Token value should be '-42'", tokens.get(0).getValue(), is("-42"));
    }

    @Test
    public void shouldTokenizeDouble() throws Exception {
        ReaderStream stream = ReaderStream.readString("42.42");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '42.42'", tokens.get(0).getValue(), is("42.42"));
    }

    @Test
    public void shouldTokenizeNegativeDouble() throws Exception {
        ReaderStream stream = ReaderStream.readString("-42.42");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '-42.42'", tokens.get(0).getValue(), is("-42.42"));
    }

    @Test
    public void shouldTokenizeDoubleEvenWhenFractionalPartIsZero() throws Exception {
        ReaderStream stream = ReaderStream.readString("42.0");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '42.0'", tokens.get(0).getValue(), is("42.0"));
    }

    @Test
    public void shouldTokenizeNegativeDoubleEvenWhenFractionalPartIsZero() throws Exception {
        ReaderStream stream = ReaderStream.readString("-42.0");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be double", tokens.get(0).getType(), is(Token.TokenType.Double));
        assertThat("Token value should be '-42.0'", tokens.get(0).getValue(), is("-42.0"));
    }

    @Test
    public void shouldTokenizeString() throws Exception {
        ReaderStream stream = ReaderStream.readString("\"string\"");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be 'string'", tokens.get(0).getValue(), is("string"));
    }

    @Test(expected = UnterminatedStringException.class)
    public void shouldFailOnUnterminatedString() throws Exception {
        ReaderStream stream = ReaderStream.readString("\"string string2");

        stream.tokenize();
    }

    @Test
    public void shouldTokenizeOpenParens() throws Exception {
        ReaderStream stream = ReaderStream.readString("(");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
    }

    @Test
    public void shouldTokenizeCloseParens() throws Exception {
        ReaderStream stream = ReaderStream.readString(")");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be CloseParens", tokens.get(0).getType(), is(Token.TokenType.CloseParens));
    }

}