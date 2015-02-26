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
    public void shouldTokenizeStringWithQuote() throws Exception {
        ReaderStream stream = ReaderStream.readString("\"str\\\"ing\"");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be 'string'", tokens.get(0).getValue(), is("str\"ing"));
    }

    @Test
    public void shouldTokenizeStringWithEscapes() throws Exception {
        ReaderStream stream = ReaderStream.readString("\"s\\t\\r\\\"i\\n\\g\"");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have one token", tokens.size(), is(1));
        assertThat("Token type should be string", tokens.get(0).getType(), is(Token.TokenType.String));
        assertThat("Token value should be 'string'", tokens.get(0).getValue(), is("s\t\r\"i\ng"));
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

    @Test
    public void shouldTokenizeSpecialChars() throws Exception {
        ReaderStream stream = ReaderStream.readString("'#");
        stream.registerSpecial('\'');
        stream.registerSpecial('#');
        List<Token> tokens = stream.tokenize();

        assertThat("Should have two tokens", tokens.size(), is(2));
        assertThat("Token type should be special", tokens.get(0).getType(), is(Token.TokenType.Special));
        assertThat("Token type should be special", tokens.get(1).getType(), is(Token.TokenType.Special));
        assertThat("Token value should be '", tokens.get(0).getValue(), is("'"));
        assertThat("Token value should be #", tokens.get(1).getValue(), is("#"));
    }

    @Test
    public void shouldTokenizeOneElementList() throws Exception {
        ReaderStream stream = ReaderStream.readString("( symbol )");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have 3 tokens", tokens.size(), is(3));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be CloseParens", tokens.get(2).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeSymbolWithHyphen() throws Exception {
        ReaderStream stream = ReaderStream.readString(":symbol-two");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have 1 token", tokens.size(), is(1));
        assertThat("Token type should be symbol", tokens.get(0).getType(), is(Token.TokenType.Symbol));
    }

    @Test
    public void shouldTokenizeMultiElementList() throws Exception {
        ReaderStream stream = ReaderStream.readString("( symbol \"String\" 15)");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have 5 tokens", tokens.size(), is(5));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be string", tokens.get(2).getType(), is(Token.TokenType.String));
        assertThat("Token type should be integer", tokens.get(3).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be CloseParens", tokens.get(4).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeMultiElementList2() throws Exception {
        ReaderStream stream = ReaderStream.readString("( symbol \"String\" 15 -33.2)");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have 6 tokens", tokens.size(), is(6));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be string", tokens.get(2).getType(), is(Token.TokenType.String));
        assertThat("Token type should be integer", tokens.get(3).getType(), is(Token.TokenType.Integer));
        assertThat("Token type should be integer", tokens.get(4).getType(), is(Token.TokenType.Double));
        assertThat("Token type should be CloseParens", tokens.get(5).getType(), is(Token.TokenType.CloseParens));
    }

    @Test
    public void shouldTokenizeNestedList() throws Exception {
        ReaderStream stream = ReaderStream.readString("( symbol (\"String\") -33.2)");

        List<Token> tokens = stream.tokenize();

        assertThat("Should have 7 tokens", tokens.size(), is(7));
        assertThat("Token type should be OpenParens", tokens.get(0).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be symbol", tokens.get(1).getType(), is(Token.TokenType.Symbol));
        assertThat("Token type should be OpenParens", tokens.get(2).getType(), is(Token.TokenType.OpenParens));
        assertThat("Token type should be string", tokens.get(3).getType(), is(Token.TokenType.String));
        assertThat("Token type should be integer", tokens.get(4).getType(), is(Token.TokenType.CloseParens));
        assertThat("Token type should be integer", tokens.get(5).getType(), is(Token.TokenType.Double));
        assertThat("Token type should be CloseParens", tokens.get(6).getType(), is(Token.TokenType.CloseParens));
    }
}